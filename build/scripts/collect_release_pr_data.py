#!/usr/bin/env python3
"""Collect PR data since the previous release for release-notes generation.

Finds the most recent ``v*`` tag, lists every PR merged into ``main`` since
that tag's commit date, and saves per-PR JSON (body + issue comments + review
comments + reviews) under ``tmp/release_pr_data/``. Bot-authored overviews
(Gemini, Claude) are tagged as ``overviews`` for downstream consumption.

No pip dependencies — stdlib only. Requires ``gh`` (authenticated) and ``git``.
"""

import argparse
import json
import subprocess
import sys
from datetime import datetime
from pathlib import Path
from typing import Any


OVERVIEW_AUTHORS = {
    "gemini-code-assist",
    "gemini-code-assist[bot]",
    "claude",
    "claude[bot]",
    "claude-code",
    "claude-code[bot]",
    "anthropic-claude",
    "anthropic-claude[bot]",
}


def run(cmd: list[str], check: bool = True) -> str:
    result = subprocess.run(cmd, capture_output=True, text=True)
    if check and result.returncode != 0:
        print(f"[err] {' '.join(cmd)}\n{result.stderr}", file=sys.stderr)
        sys.exit(1)
    return result.stdout


def latest_release_tag() -> str:
    tags = run(["git", "tag", "--list", "v*", "--sort=-v:refname"]).splitlines()
    if not tags:
        sys.exit("No v* tags found in this repo.")
    return tags[0].strip()


def tag_commit_iso(tag: str) -> str:
    return run(["git", "log", "-1", "--format=%aI", tag]).strip()


def gh_json(args: list[str]) -> Any:
    out = run(["gh", *args])
    return json.loads(out) if out.strip() else None


def list_prs_since(since_iso: str) -> list[dict]:
    fields = "number,title,author,mergedAt,url,body,labels,baseRefName"
    search = f"is:pr is:merged base:main merged:>={since_iso}"
    prs = gh_json([
        "pr", "list",
        "--state", "merged",
        "--search", search,
        "--json", fields,
        "--limit", "500",
    ]) or []
    prs.sort(key=lambda p: p.get("mergedAt") or "")
    return prs


def fetch_pr_detail(number: int) -> dict:
    return gh_json([
        "pr", "view", str(number),
        "--json",
        "number,title,author,mergedAt,url,body,labels,baseRefName,"
        "comments,reviews",
    ]) or {}


def is_overview(author_login: str, body: str) -> bool:
    if not body:
        return False
    if author_login.lower() in OVERVIEW_AUTHORS:
        return True
    markers = (
        "## Summary of Changes",
        "Summary of Changes",
        "## Claude finished",
        "Claude's review",
        "Claude Code Review",
    )
    head = body[:500]
    return any(m in head for m in markers)


def extract_overviews(detail: dict) -> list[dict]:
    found: list[dict] = []
    for c in detail.get("comments") or []:
        author = (c.get("author") or {}).get("login") or ""
        body = c.get("body") or ""
        if is_overview(author, body):
            found.append({
                "source": "issue_comment",
                "author": author,
                "created_at": c.get("createdAt"),
                "url": c.get("url"),
                "body": body,
            })
    for r in detail.get("reviews") or []:
        author = (r.get("author") or {}).get("login") or ""
        body = r.get("body") or ""
        if is_overview(author, body):
            found.append({
                "source": "review",
                "author": author,
                "state": r.get("state"),
                "created_at": r.get("submittedAt"),
                "body": body,
            })
    return found


def slim(detail: dict, overviews: list[dict]) -> dict:
    return {
        "number": detail.get("number"),
        "title": detail.get("title"),
        "url": detail.get("url"),
        "author": (detail.get("author") or {}).get("login"),
        "merged_at": detail.get("mergedAt"),
        "labels": [l.get("name") for l in detail.get("labels") or []],
        "body": detail.get("body") or "",
        "overviews": overviews,
    }


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--since-tag", help="Tag to collect PRs after (default: latest v*).")
    ap.add_argument("--out-dir", default="tmp/release_pr_data",
                    help="Output directory (default: tmp/release_pr_data)")
    args = ap.parse_args()

    tag = args.since_tag or latest_release_tag()
    since = tag_commit_iso(tag)
    print(f"[info] Previous release tag: {tag} (commit at {since})")

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    prs = list_prs_since(since)
    print(f"[info] Found {len(prs)} PRs merged since {tag}.")

    combined: list[dict] = []
    for i, pr in enumerate(prs, 1):
        num = pr["number"]
        print(f"[info] ({i}/{len(prs)}) #{num} {pr['title'][:70]}")
        detail = fetch_pr_detail(num)
        if not detail:
            continue
        overviews = extract_overviews(detail)
        record = slim(detail, overviews)
        (out_dir / f"pr_{num:05d}.json").write_text(
            json.dumps(record, indent=2, sort_keys=True) + "\n"
        )
        combined.append({
            "number": record["number"],
            "title": record["title"],
            "url": record["url"],
            "author": record["author"],
            "merged_at": record["merged_at"],
            "labels": record["labels"],
            "overview_count": len(overviews),
        })

    summary = {
        "generated_at": datetime.now().astimezone().isoformat(),
        "since_tag": tag,
        "since_commit_iso": since,
        "pr_count": len(combined),
        "prs": combined,
    }
    (out_dir / "summary.json").write_text(
        json.dumps(summary, indent=2, sort_keys=True) + "\n"
    )
    print(f"[info] Wrote {len(combined)} PR files + summary.json to {out_dir}")


if __name__ == "__main__":
    main()
