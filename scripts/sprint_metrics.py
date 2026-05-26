#!/usr/bin/env python3
"""
Extract sprint metrics from git history and agile doc state changes.

Outputs CSV files for gnuplot chart generation:
  - sprint_activity.csv: per-day PRs, commits, commits/PR, lines added/deleted
  - sprint_progress.csv: cumulative done, PR cycle times
"""

import argparse
import csv
import datetime
import os
import re
import subprocess
import sys
from collections import defaultdict
from pathlib import Path


def parse_args() -> argparse.Namespace:
    ap = argparse.ArgumentParser(description="Extract sprint metrics")
    ap.add_argument("--sprint", type=int, required=True, help="Sprint number (e.g. 18)")
    ap.add_argument("--start-date", help="Override auto-detected start date (YYYY-MM-DD)")
    ap.add_argument("--end-date", help="Override auto-detected end date (YYYY-MM-DD)")
    ap.add_argument("--output-dir", default=None, help="Output directory (default: build/output/sprint_N)")
    return ap.parse_args()


def run(cmd: list[str]) -> str:
    return subprocess.check_output(cmd, text=True).strip()


def parse_date(s: str) -> datetime.date:
    parts = s.strip().split("-")
    return datetime.date(int(parts[0]), int(parts[1]), int(parts[2]))


def get_sprint_range(sprint: int) -> tuple[datetime.date, datetime.date]:
    """Read sprint start/end from the sprint doc's Status table."""
    sprint_file = Path(f"doc/agile/versions/v0/sprint_{sprint:02d}/sprint.org")
    if not sprint_file.exists():
        print(f"Warning: {sprint_file} not found, estimating dates from git", file=sys.stderr)
        return _estimate_dates(sprint)
    
    text = sprint_file.read_text()
    start = end = None
    
    for line in text.splitlines():
        if line.startswith("| Start") and "|" in line:
            m = re.search(r"\d{4}-\d{2}-\d{2}", line)
            if m:
                start = parse_date(m.group())
        if "End" in line and "|" in line:
            m = re.search(r"\d{4}-\d{2}-\d{2}", line)
            if m:
                end = parse_date(m.group())
    
    if start and end:
        return start, end
    
    # Fallback: use Last touched date
    for line in text.splitlines():
        if "Last touched" in line and "|" in line:
            m = re.search(r"\d{4}-\d{2}-\d{2}", line)
            if m:
                end = parse_date(m.group())
    if not end:
        print(f"Warning: could not parse sprint dates from {sprint_file}", file=sys.stderr)
        return _estimate_dates(sprint)
    
    m = re.search(r"#\+created:\s*(\d{4}-\d{2}-\d{2})", text)
    start = parse_date(m.group(1)) if m else (end - datetime.timedelta(days=7))
    return start, end


def _estimate_dates(sprint: int) -> tuple[datetime.date, datetime.date]:
    """Fallback date estimation when sprint doc is unavailable."""
    now = datetime.datetime.now().date()
    return now - datetime.timedelta(days=14), now


def collect_daily_metrics(start: datetime.date, end: datetime.date) -> dict:
    """Get commits and merges per day between start and end."""
    metrics = defaultdict(lambda: {"commits": 0, "merges": 0, "added": 0, "deleted": 0})
    
    end_inc = end + datetime.timedelta(days=1)
    
    # Single pass: get commit date and numstat together so we don't re-query per file
    log = run([
        "git", "log", f"--after={start}", f"--before={end_inc}",
        "--format=COMMIT %ai", "--numstat", "--reverse", "origin/main"
    ])
    
    current_date = None
    for line in log.splitlines():
        if line.startswith("COMMIT "):
            d_str = line.split()[1]
            current_date = parse_date(d_str)
            if current_date:
                metrics[current_date]["commits"] += 1
                # Count merge commits (those with "Merge" in message)
                # We detect merges by grep in the raw output
        elif current_date and "\t" in line and not line.startswith("COMMIT"):
            parts = line.split("\t")
            if len(parts) == 3 and parts[0] != "-" and parts[0] != "":
                try:
                    metrics[current_date]["added"] += int(parts[0])
                    metrics[current_date]["deleted"] += int(parts[1])
                except (ValueError, KeyError):
                    pass
    
    # Merges per day (separate pass — simpler)
    log = run([
        "git", "log", "--merges", f"--after={start}", f"--before={end_inc}",
        "--format=%ai", "--reverse", "origin/main"
    ])
    for line in log.splitlines():
        d = parse_date(line.split()[0])
        metrics[d]["merges"] += 1
    
    return metrics


def get_merged_prs(start: datetime.date, end: datetime.date) -> list[dict]:
    """Get all PRs merged in the date range using gh CLI."""
    gh_before = end + datetime.timedelta(days=1)
    prs = []
    try:
        # gh search prs handles date ranges better than gh pr list
        output = run([
            "gh", "search", "prs",
            f"repo=OreStudio/OreStudio",
            f"merged:{start}..{gh_before}",
            "--json", "number,title,createdAt,closedAt",
            "--limit", "200"
        ])
    except subprocess.CalledProcessError as e:
        print(f"Warning: gh search prs failed: {e}", file=sys.stderr)
        return prs
    
    import json
    try:
        data = json.loads(output)
    except json.JSONDecodeError:
        return prs
    
    for pr in data:
        created = parse_date(pr["createdAt"][:10])
        closed = parse_date(pr["closedAt"][:10])
        cycle_hours = (closed - created).days * 24
        commit_count = 0  # not available via gh search prs
        prs.append({
            "number": pr["number"],
            "title": pr["title"][:60],
            "merged_day": closed,
            "cycle_hours": cycle_hours,
            "commit_count": commit_count,
        })
    return prs


def collect_story_progress(start: datetime.date, end: datetime.date) -> list[dict]:
    """Track STARTED→DONE transitions in story docs."""
    log = run([
        "git", "log", f"--after={start}", f"--before={end + datetime.timedelta(days=1)}",
        "-p", "--diff-filter=M", "--date=short", "--",
        "doc/agile/versions/v0/sprint_*/**/story.org"
    ])
    
    transitions = []
    current_date = None
    
    for line in log.splitlines():
        if line.startswith("Date:"):
            d_str = line.split(":", 1)[1].strip()
            # Parse ISO date (YYYY-MM-DD) after using --date=short
            current_date = parse_date(d_str.split()[0])
        elif "STARTED" in line and "DONE" in line and current_date:
            transitions.append({"date": current_date, "story": "unknown"})
    
    # Cumulative by day
    cumulative = defaultdict(int)
    for t in transitions:
        cumulative[t["date"]] += 1
    return cumulative


def write_sprint_activity(metrics: dict, prs: list[dict], output_dir: Path):
    """Write sprint_activity.csv: day, merges, commits, commits_per_pr, added, deleted."""
    path = output_dir / "sprint_activity.csv"
    days = sorted(metrics.keys())
    
    # Compute commits/PR per day
    commits_per_pr = {}
    for pr in prs:
        d = pr["merged_day"]
        if d in commits_per_pr:
            commits_per_pr[d].append(pr["commit_count"])
        else:
            commits_per_pr[d] = [pr["commit_count"]]
    
    with open(path, "w", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(["day", "merges", "commits", "commits_per_pr", "added", "deleted"])
        for d in days:
            m = metrics[d]
            avg_cpp = ""
            if d in commits_per_pr:
                vals = commits_per_pr[d]
                avg_cpp = f"{sum(vals)/len(vals):.1f}"
            w.writerow([d.isoformat(), m["merges"], m["commits"], avg_cpp, m["added"], m["deleted"]])
    print(f"Wrote {path}")


def write_sprint_progress(cumulative: dict, prs: list[dict], output_dir: Path):
    """Write sprint_progress.csv: day, cumulative_stories_done, pr_number, cycle_hours."""
    act_path = output_dir / "sprint_progress.csv"
    
    # Cumulative stories done
    with open(act_path, "w", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(["day", "cumulative_done"])
        days = sorted(cumulative.keys())
        running = 0
        for d in days:
            running += cumulative[d]
            w.writerow([d.isoformat(), running])
    
    # PR cycle times (separate section in same CSV, or separate file)
    cycle_path = output_dir / "pr_cycle_times.csv"
    with open(cycle_path, "w", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(["pr_number", "title", "day", "cycle_hours", "commit_count"])
        for pr in sorted(prs, key=lambda x: x["merged_day"]):
            w.writerow([pr["number"], pr["title"], pr["merged_day"].isoformat(),
                        pr["cycle_hours"], pr["commit_count"]])
    print(f"Wrote {cycle_path}")
    print(f"Wrote {cycle_path.parent / 'pr_cycle_times.csv'}")


def main():
    args = parse_args()
    sprint = args.sprint
    
    start, end = get_sprint_range(sprint)
    if args.start_date:
        start = parse_date(args.start_date)
    if args.end_date:
        end = parse_date(args.end_date)
    
    print(f"Sprint {sprint}: {start} → {end}")
    
    output_dir = Path(args.output_dir) if args.output_dir else Path(f"build/output/sprint_{sprint}")
    output_dir.mkdir(parents=True, exist_ok=True)
    
    daily = collect_daily_metrics(start, end)
    prs = get_merged_prs(start, end)
    cumulative = collect_story_progress(start, end)
    
    write_sprint_activity(daily, prs, output_dir)
    write_sprint_progress(cumulative, prs, output_dir)
    
    print(f"Found {len(daily)} days of activity, {len(prs)} merged PRs, {len(cumulative)} story transitions")


if __name__ == "__main__":
    main()
