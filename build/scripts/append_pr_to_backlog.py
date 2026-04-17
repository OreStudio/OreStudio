#!/usr/bin/env python3
"""Append merged PRs as auto-generated stories to the latest sprint backlog.

Two modes:

* ``--pr <N>``     — process one PR (intended for PR-closure workflow).
* ``--since-tag``  — process every PR merged to ``main`` since a tag
                     (defaults to the most recent ``v*`` tag). Intended for
                     one-shot catch-up when closing a sprint.

Each PR becomes a ``*** COMPLETED <title> :<tag>:`` story under a new
``* Unprocessed Stories`` section at the end of the latest sprint backlog
(before ``* Footer`` if present). Descriptions come from the Gemini / Claude
overview (or PR body); tasks come from commit headlines; clocking is
estimated from author timestamps using a session-gap heuristic and emitted
as ``CLOCK:`` lines inside a ``:LOGBOOK:`` drawer.

The script is idempotent: PRs whose number already appears as a
``:PR_NUMBER:`` property in the target backlog are skipped.

Stdlib only. Requires ``gh`` (authenticated) and ``git``.
"""

import argparse
import json
import re
import subprocess
import sys
from collections import defaultdict
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Iterable


OVERVIEW_AUTHORS = {
    "gemini-code-assist", "gemini-code-assist[bot]",
    "claude", "claude[bot]",
    "claude-code", "claude-code[bot]",
    "anthropic-claude", "anthropic-claude[bot]",
}

SESSION_GAP_MINUTES = 90   # gaps longer than this are treated as breaks
SESSION_TAIL_MINUTES = 20  # baseline effort attributed to each coding session
MAX_DAY_MINUTES = 8 * 60   # cap any single day at 8h

# ---------------------------------------------------------------------------
# Shell helpers
# ---------------------------------------------------------------------------

def run(cmd: list[str], check: bool = True) -> str:
    r = subprocess.run(cmd, capture_output=True, text=True)
    if check and r.returncode != 0:
        sys.exit(f"[err] {' '.join(cmd)}\n{r.stderr}")
    return r.stdout


def gh_json(args: list[str]):
    out = run(["gh", *args])
    return json.loads(out) if out.strip() else None


# ---------------------------------------------------------------------------
# Backlog discovery
# ---------------------------------------------------------------------------

def latest_backlog(root: Path) -> Path:
    candidates = sorted(root.glob("doc/agile/v0/sprint_backlog_*.org"))
    numbered = []
    for p in candidates:
        m = re.search(r"sprint_backlog_(\d+)\.org$", p.name)
        if m:
            numbered.append((int(m.group(1)), p))
    if not numbered:
        sys.exit("No sprint_backlog_*.org found under doc/agile/v0/")
    return max(numbered)[1]


# ---------------------------------------------------------------------------
# PR discovery and fetch
# ---------------------------------------------------------------------------

PR_FIELDS = (
    "number,title,author,mergedAt,url,body,labels,comments,reviews,commits"
)


def fetch_pr(number: int) -> dict:
    return gh_json(["pr", "view", str(number), "--json", PR_FIELDS]) or {}


def list_prs_since(tag: str) -> list[int]:
    iso = run(["git", "log", "-1", "--format=%aI", tag]).strip()
    search = f"is:pr is:merged base:main merged:>={iso}"
    prs = gh_json([
        "pr", "list", "--state", "merged",
        "--search", search, "--json", "number,mergedAt",
        "--limit", "500",
    ]) or []
    prs.sort(key=lambda p: p.get("mergedAt") or "")
    return [p["number"] for p in prs]


def latest_release_tag() -> str:
    tags = run(["git", "tag", "--list", "v*", "--sort=-v:refname"]).splitlines()
    if not tags:
        sys.exit("No v* tags found.")
    return tags[0].strip()


# ---------------------------------------------------------------------------
# Content extraction
# ---------------------------------------------------------------------------

def extract_overview(pr: dict) -> str:
    """First substantive paragraph from a bot overview, or from PR body."""
    for source in ("reviews", "comments"):
        for entry in pr.get(source) or []:
            author = (entry.get("author") or {}).get("login") or ""
            if author.lower() not in OVERVIEW_AUTHORS:
                continue
            body = entry.get("body") or ""
            # Strip the Gemini "## Code Review" header and everything after the
            # first section break.
            body = re.sub(r"^#+\s*Code Review\s*\n+", "", body.strip())
            m = re.match(r"(.*?)(?:\n##|\n---)", body, re.S)
            chunk = (m.group(1) if m else body).strip()
            if chunk:
                return chunk
    body = (pr.get("body") or "").strip()
    if not body:
        return ""
    # Drop HTML-detail blocks (Dependabot) and Co-Authored-By footers.
    body = re.sub(r"<details>.*?</details>", "", body, flags=re.S)
    body = re.sub(r"(?m)^Co-Authored-By:.*$", "", body)
    body = re.sub(r"🤖 Generated with.*$", "", body, flags=re.S)
    paragraphs = [p.strip() for p in body.split("\n\n") if p.strip()]
    return "\n\n".join(paragraphs[:2]).strip()


INFRA_PREFIXES = {
    "build", "cmake", "windows", "infra", "ci", "valgrind", "nats",
    "platform",
}
AGILE_PREFIXES = {"agile", "doc"}
ANALYSIS_PREFIXES = {"analysis", "plan"}


def story_tag(title: str) -> str:
    """Pick :code:, :infra:, :agile:, or :analysis: from the [prefix]."""
    if title.startswith("Bump "):
        return "infra"
    m = re.match(r"\[([^\]]+)\]", title)
    if not m:
        return "code"
    prefixes = {p.strip().lower() for p in m.group(1).split(",")}
    if prefixes & AGILE_PREFIXES:
        return "agile"
    if prefixes & ANALYSIS_PREFIXES:
        return "analysis"
    if prefixes & INFRA_PREFIXES:
        return "infra"
    return "code"


# ---------------------------------------------------------------------------
# Clock estimation
# ---------------------------------------------------------------------------

def parse_iso(s: str) -> datetime:
    # GitHub returns trailing Z; fromisoformat handles +00:00 but not Z on older Pythons.
    return datetime.fromisoformat(s.replace("Z", "+00:00"))


def full_headline(c: dict) -> str:
    """Reconstruct a commit's full subject from truncated messageHeadline."""
    head = (c.get("messageHeadline") or "").rstrip()
    body = (c.get("messageBody") or "")
    if head.endswith("…") and body.startswith("…"):
        tail = body.split("\n", 1)[0]
        return head[:-1] + tail[1:]
    return head


def estimate_minutes_per_day(commit_times: list[datetime]) -> dict:
    """Return {date: minutes} for the day-bucketed session estimate."""
    by_day: dict[object, list[datetime]] = defaultdict(list)
    for t in commit_times:
        by_day[t.date()].append(t)
    per_day = {}
    for day, ts in by_day.items():
        ts.sort()
        if len(ts) == 1:
            minutes = SESSION_TAIL_MINUTES
        else:
            minutes = SESSION_TAIL_MINUTES  # baseline for first session
            for a, b in zip(ts, ts[1:]):
                gap = (b - a).total_seconds() / 60
                if gap <= SESSION_GAP_MINUTES:
                    minutes += gap
                else:
                    minutes += SESSION_TAIL_MINUTES  # cost of resuming
        per_day[day] = int(min(minutes, MAX_DAY_MINUTES))
    return per_day


def round_to_5(dt: datetime) -> datetime:
    minute = (dt.minute // 5) * 5
    return dt.replace(minute=minute, second=0, microsecond=0)


def clock_lines(commits: list[dict]) -> tuple[list[str], int]:
    """Return (CLOCK: lines, total minutes). Times are local (no TZ in org)."""
    times = [parse_iso(c["authoredDate"]).astimezone() for c in commits]
    if not times:
        return [], 0
    per_day = estimate_minutes_per_day(times)
    lines: list[str] = []
    total = 0
    by_day: dict[object, list[datetime]] = defaultdict(list)
    for t in times:
        by_day[t.date()].append(t)
    for day in sorted(by_day):
        minutes = per_day[day]
        total += minutes
        last = max(by_day[day])
        end = round_to_5(last)
        start = end - timedelta(minutes=minutes)
        fmt = "%Y-%m-%d %a %H:%M"
        h, m = divmod(minutes, 60)
        lines.append(
            f"CLOCK: [{start.strftime(fmt)}]--[{end.strftime(fmt)}] => "
            f"{h:2d}:{m:02d}"
        )
    return lines, total


# ---------------------------------------------------------------------------
# Story block rendering
# ---------------------------------------------------------------------------

def render_story(pr: dict) -> str:
    number = pr["number"]
    title = pr["title"].replace("\n", " ").strip()
    tag = story_tag(title)
    url = pr["url"]
    author = (pr.get("author") or {}).get("login") or ""
    merged = pr.get("mergedAt") or ""
    overview = extract_overview(pr) or "(No overview — auto-generated; please curate.)"
    commits = pr.get("commits") or []
    logbook, total = clock_lines(commits)

    # Task list: one per commit headline (skip merge commits), plus a PR-merge task.
    tasks: list[str] = []
    for c in commits:
        headline = full_headline(c)
        if not headline or headline.lower().startswith("merge "):
            continue
        tasks.append(f"- [X] {headline}")
    tasks.append(f"- [X] Merged in [[{url}][PR #{number}]]")

    h, m = divmod(total, 60)
    est = f"{h}h {m:02d}m"

    out = [
        f"*** COMPLETED {title}                                  :{tag}:",
        ":PROPERTIES:",
        f":PR_NUMBER: {number}",
        f":PR_URL: {url}",
        f":PR_AUTHOR: {author}",
        f":PR_MERGED: {merged}",
        f":TIME_ESTIMATE: {est}",
        ":END:",
        "",
        overview,
        "",
    ]
    if logbook:
        out.append(":LOGBOOK:")
        out.extend(logbook)
        out.append(":END:")
        out.append("")
    out.append("***** Tasks")
    out.append("")
    out.extend(tasks)
    out.append("")
    return "\n".join(out)


# ---------------------------------------------------------------------------
# Backlog mutation
# ---------------------------------------------------------------------------

UNPROCESSED_HEADER = (
    "* Unprocessed Stories\n"
    "\n"
    "Auto-generated from merged pull requests. Titles, descriptions, tags,\n"
    "tasks, and clock estimates are heuristic and need manual review before\n"
    "the sprint is closed.\n\n"
)


def existing_pr_numbers(text: str) -> set[int]:
    return {int(m) for m in re.findall(r"^:PR_NUMBER:\s*(\d+)\s*$", text, re.M)}


def insert_story(backlog_text: str, story: str) -> str:
    """Append ``story`` under ``* Unprocessed Stories``; create it if absent."""
    section_re = re.compile(r"(?m)^\* Unprocessed Stories\s*$")
    footer_re = re.compile(r"(?m)^\* Footer\s*$")

    if section_re.search(backlog_text):
        # Find end of Unprocessed Stories (before next top-level * or EOF).
        m = section_re.search(backlog_text)
        section_start = m.end()
        next_top = re.search(r"(?m)^\* ", backlog_text[section_start:])
        insert_at = section_start + (next_top.start() if next_top else len(backlog_text) - section_start)
        return backlog_text[:insert_at] + story + "\n" + backlog_text[insert_at:]

    # Create the section. Place it just before * Footer if present, else at EOF.
    new_section = "\n" + UNPROCESSED_HEADER + story + "\n"
    m = footer_re.search(backlog_text)
    if m:
        return backlog_text[:m.start()] + new_section + "\n" + backlog_text[m.start():]
    if not backlog_text.endswith("\n"):
        backlog_text += "\n"
    return backlog_text + new_section


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def collect_pr_numbers(args: argparse.Namespace) -> list[int]:
    if args.pr:
        return [args.pr]
    tag = args.since_tag or latest_release_tag()
    print(f"[info] Collecting PRs merged since tag {tag}")
    return list_prs_since(tag)


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    mode = ap.add_mutually_exclusive_group(required=False)
    mode.add_argument("--pr", type=int, help="Single PR number.")
    mode.add_argument("--since-tag", help="Process every PR merged since this tag.")
    ap.add_argument("--backlog", type=Path,
                    help="Target backlog file (default: latest under doc/agile/v0).")
    ap.add_argument("--root", type=Path, default=Path.cwd(),
                    help="Project root (default: cwd).")
    ap.add_argument("--dry-run", action="store_true",
                    help="Print the story block(s) but don't modify the backlog.")
    args = ap.parse_args()

    backlog = args.backlog or latest_backlog(args.root)
    print(f"[info] Target backlog: {backlog}")
    text = backlog.read_text()
    seen = existing_pr_numbers(text)

    numbers = collect_pr_numbers(args)
    if not numbers:
        print("[info] No PRs to process.")
        return

    stories: list[str] = []
    appended = 0
    skipped = 0
    for i, n in enumerate(numbers, 1):
        if n in seen:
            skipped += 1
            continue
        print(f"[info] ({i}/{len(numbers)}) fetching PR #{n}")
        pr = fetch_pr(n)
        if not pr:
            print(f"[warn]   PR #{n} not found, skipping")
            continue
        stories.append(render_story(pr))
        seen.add(n)
        appended += 1

    if args.dry_run:
        print("\n".join(stories))
        print(f"\n[dry-run] would append {appended} stories; skipped {skipped} already present.")
        return

    new_text = text
    for s in stories:
        new_text = insert_story(new_text, s)
    if new_text != text:
        backlog.write_text(new_text)
    print(f"[info] Appended {appended} stories; skipped {skipped} already present.")


if __name__ == "__main__":
    main()
