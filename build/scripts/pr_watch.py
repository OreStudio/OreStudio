#!/usr/bin/env python3
"""PR Watcher — background CI and review monitor for a GitHub pull request.

Polls at a fixed interval and invokes ``claude -p`` only when actionable
events are detected:

  * One or more CI check-runs have failed on the current head SHA
    (tracked by run ID so the same failure is not re-triggered).
  * One or more review threads are open, unresolved, and not authored
    by an ignored login (tracked by thread ID).
  * All CI check-runs pass and no open threads remain → exits cleanly.

No pip dependencies — stdlib only.
Requires: Python 3.10+, ``gh`` CLI (authenticated), ``claude`` CLI.
"""

import argparse
import json
import re
import subprocess
import sys
import time
from typing import Any


# ---------------------------------------------------------------------------
# GitHub helpers
# ---------------------------------------------------------------------------

def _gh(*args: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        ["gh", *args], capture_output=True, text=True,
    )


def gh_api(endpoint: str, repo: str | None = None) -> Any:
    """Call ``gh api`` and return parsed JSON, or None on failure."""
    if repo and not endpoint.startswith("/"):
        endpoint = f"/repos/{repo}/{endpoint}"
    result = _gh("api", endpoint)
    if result.returncode != 0:
        print(f"[warn] gh api {endpoint}: {result.stderr.strip()}", file=sys.stderr)
        return None
    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError:
        return None


def gh_graphql(query: str) -> Any:
    """Call ``gh api graphql`` and return parsed JSON, or None on failure."""
    result = _gh("api", "graphql", "-f", f"query={query}")
    if result.returncode != 0:
        print(f"[warn] graphql failed: {result.stderr.strip()}", file=sys.stderr)
        return None
    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError:
        return None


def get_repo_slug() -> str:
    """Derive owner/repo from git remote origin."""
    result = subprocess.run(
        ["git", "remote", "get-url", "origin"],
        capture_output=True, text=True, check=True,
    )
    url = result.stdout.strip()
    m = re.search(r"[/:]([^/:]+/[^/:]+?)(?:\.git)?$", url)
    if not m:
        sys.exit(f"Cannot parse repo slug from remote: {url}")
    return m.group(1)


def get_current_gh_login() -> str:
    """Return the authenticated gh CLI username."""
    result = _gh("api", "user", "--jq", ".login")
    if result.returncode == 0:
        return result.stdout.strip()
    return ""


# ---------------------------------------------------------------------------
# PR data
# ---------------------------------------------------------------------------

def get_pr_head_sha(pr: int, repo: str) -> str:
    data = gh_api(f"pulls/{pr}", repo)
    if data:
        return data.get("head", {}).get("sha", "")
    return ""


def get_pr_branch(pr: int, repo: str) -> str:
    data = gh_api(f"pulls/{pr}", repo)
    if data:
        return data.get("head", {}).get("ref", "unknown")
    return "unknown"


def get_ci_checks(sha: str, repo: str) -> list[dict]:
    """Return all check-runs for the given commit SHA."""
    data = gh_api(f"commits/{sha}/check-runs?per_page=100", repo)
    if not data or "check_runs" not in data:
        return []
    return data["check_runs"]


def get_failed_run_log(run_id: int, max_chars: int = 3000) -> str:
    """Fetch the failure log for a GitHub Actions run."""
    result = subprocess.run(
        ["gh", "run", "view", str(run_id), "--log-failed"],
        capture_output=True, text=True,
    )
    text = result.stdout or result.stderr
    return text[:max_chars] if text else "(no log available)"


def get_open_threads(pr: int, repo: str, ignore_logins: set[str]) -> list[dict]:
    """Return unresolved review threads not opened by an ignored login."""
    owner, name = repo.split("/", 1)
    query = f"""
    {{
      repository(owner: "{owner}", name: "{name}") {{
        pullRequest(number: {pr}) {{
          reviewThreads(first: 50) {{
            nodes {{
              id
              isResolved
              comments(first: 1) {{
                nodes {{
                  author {{ login }}
                  body
                  path
                  line
                }}
              }}
            }}
          }}
        }}
      }}
    }}
    """
    data = gh_graphql(query)
    if not data:
        return []

    nodes = (
        data.get("data", {})
            .get("repository", {})
            .get("pullRequest", {})
            .get("reviewThreads", {})
            .get("nodes", [])
    )
    result = []
    for node in nodes:
        if node.get("isResolved"):
            continue
        comments = node.get("comments", {}).get("nodes", [])
        if not comments:
            continue
        author = comments[0].get("author", {}).get("login", "")
        if author in ignore_logins:
            continue
        result.append(node)
    return result


# ---------------------------------------------------------------------------
# Claude prompts
# ---------------------------------------------------------------------------

def build_ci_prompt(pr: int, branch: str, failures: list[dict]) -> str:
    lines = [
        f"PR #{pr} (branch: {branch}) has CI failures that need to be fixed.",
        "",
        "Failed check-runs:",
    ]
    for f in failures:
        name = f.get("name", "unknown")
        url = f.get("html_url", "")
        run_id = f.get("id", 0)
        lines.append(f"  - {name}  {url}")
        log = get_failed_run_log(run_id)
        lines.append(f"    Log excerpt:\n{log}")
        lines.append("")
    lines += [
        "Please investigate the failures, fix the root cause, commit, and push.",
    ]
    return "\n".join(lines)


def build_review_prompt(pr: int, branch: str, threads: list[dict]) -> str:
    lines = [
        f"PR #{pr} (branch: {branch}) has unresolved review comments to address.",
        "",
        "Open threads:",
    ]
    for t in threads:
        tid = t.get("id", "")
        comments = t.get("comments", {}).get("nodes", [])
        if not comments:
            continue
        c = comments[0]
        author = c.get("author", {}).get("login", "")
        body = c.get("body", "").strip()
        path = c.get("path") or ""
        line = c.get("line") or ""
        location = f"{path}:{line}" if path else "(general)"
        lines.append(f"  Thread {tid} — {author} at {location}:")
        lines.append(f"    {body}")
        lines.append("")
    lines += [
        "For each thread: apply the fix, commit, push, reply to the thread on",
        "GitHub citing the fix commit, and resolve the thread.",
    ]
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Watch loop
# ---------------------------------------------------------------------------

def invoke_claude(prompt: str) -> None:
    print(f"[claude] invoking claude -p ({len(prompt)} chars) ...", flush=True)
    subprocess.run(["claude", "-p", prompt], check=False)


def watch(pr: int, interval_s: int, repo: str, ignore_logins: set[str]) -> None:
    branch = get_pr_branch(pr, repo)
    print(
        f"[pr-watch] PR #{pr} ({branch}) in {repo} "
        f"— polling every {interval_s // 60}m, ignoring {ignore_logins}",
        flush=True,
    )

    seen_run_ids: set[int] = set()
    seen_thread_ids: set[str] = set()
    iteration = 0

    while True:
        iteration += 1
        ts = time.strftime("%H:%M:%S")
        print(f"\n[{ts}] poll #{iteration}", flush=True)

        sha = get_pr_head_sha(pr, repo)
        if not sha:
            print(f"[{ts}] could not fetch PR head SHA, retrying next interval",
                  flush=True)
            time.sleep(interval_s)
            continue

        # ── CI checks ──────────────────────────────────────────────────
        checks = get_ci_checks(sha, repo)
        pending = [c for c in checks if c.get("status") != "completed"]
        new_failures = [
            c for c in checks
            if c.get("conclusion") == "failure"
            and c.get("id") not in seen_run_ids
        ]
        all_passed = (
            bool(checks)
            and not pending
            and all(c.get("conclusion") == "success" for c in checks)
        )

        ci_summary = (
            f"{sum(1 for c in checks if c.get('conclusion') == 'success')} passed, "
            f"{len(new_failures)} new failures, "
            f"{len(pending)} pending"
        )
        print(f"[{ts}] CI: {ci_summary}", flush=True)

        if new_failures:
            for f in new_failures:
                seen_run_ids.add(f["id"])
            invoke_claude(build_ci_prompt(pr, branch, new_failures))

        # ── Review threads ─────────────────────────────────────────────
        open_threads = get_open_threads(pr, repo, ignore_logins)
        new_threads = [t for t in open_threads if t["id"] not in seen_thread_ids]

        print(f"[{ts}] review threads: {len(open_threads)} open, "
              f"{len(new_threads)} new", flush=True)

        if new_threads:
            for t in new_threads:
                seen_thread_ids.add(t["id"])
            invoke_claude(build_review_prompt(pr, branch, new_threads))

        # ── Done? ──────────────────────────────────────────────────────
        if all_passed and not open_threads:
            print(
                f"[{ts}] All CI checks green and no open review threads. "
                f"PR #{pr} is ready.",
                flush=True,
            )
            break

        time.sleep(interval_s)


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Poll a PR for CI failures and review comments; "
                    "invoke claude only when actionable.",
    )
    parser.add_argument("pr", type=int, help="Pull request number")
    parser.add_argument(
        "--interval", type=int, default=5,
        metavar="MINUTES",
        help="Polling interval in minutes (default: 5)",
    )
    parser.add_argument(
        "--repo", default=None,
        metavar="OWNER/REPO",
        help="Repository slug (default: derived from git remote)",
    )
    parser.add_argument(
        "--ignore-logins", default=None,
        metavar="LOGINS",
        help="Comma-separated logins to skip when scanning review threads "
             "(default: current authenticated gh user)",
    )
    args = parser.parse_args()

    repo = args.repo or get_repo_slug()

    if args.ignore_logins:
        ignore_logins = {s.strip() for s in args.ignore_logins.split(",")}
    else:
        me = get_current_gh_login()
        ignore_logins = {me} if me else set()

    watch(args.pr, args.interval * 60, repo, ignore_logins)


if __name__ == "__main__":
    main()
