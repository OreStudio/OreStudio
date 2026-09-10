"""compass skills — the skill-selection instrument.

Selections are recorded by a PreToolUse hook on the Skill tool (see
doc/llm/claude_code_settings.org), so the measurement accrues without
anyone remembering to record it. Corrections are a judgement, so they are
recorded explicitly with 'compass skills correct'.

The log is append-only JSONL in the git common directory, which every
worktree in the fleet shares, so the baseline is project-wide rather than
per-checkout.
"""

import argparse
import json
import os
import re
import subprocess
import sys
import uuid
from collections import Counter
from datetime import datetime, timedelta, timezone
from pathlib import Path

LOG_NAME = "skill_selections.jsonl"
CAUSES = ("description", "catalogue-size", "level", "none")
DEFAULT_WINDOW = "14d"

SKILL_ROOTS = (
    Path(".claude") / "skills",
    Path.home() / ".claude" / "skills",
    Path.home() / ".claude" / "plugins" / "cache",
)


def log_path(project_root):
    """The shared log, in the git common dir so every worktree appends to one
    file. Falls back to the worktree when git cannot answer."""
    try:
        p = subprocess.run(["git", "rev-parse", "--git-common-dir"],
                           cwd=project_root, capture_output=True, text=True)
        if p.returncode == 0 and p.stdout.strip():
            common = Path(p.stdout.strip())
            if not common.is_absolute():
                common = Path(project_root) / common
            return common / LOG_NAME
    except OSError:
        pass
    return Path(project_root) / LOG_NAME


def count_visible_skills(project_root):
    """How many skills the agent could choose from: project, user and plugin
    catalogues, counted by their SKILL.md files."""
    total = 0
    for root in SKILL_ROOTS:
        base = root if root.is_absolute() else Path(project_root) / root
        if not base.is_dir():
            continue
        total += sum(1 for _ in base.glob("**/SKILL.md"))
    return total


def environment_label(project_root):
    env_file = Path(project_root) / ".env"
    try:
        for line in env_file.read_text(encoding="utf-8").splitlines():
            if line.startswith("ORES_CHECKOUT_LABEL="):
                return line.split("=", 1)[1].strip()
    except OSError:
        pass
    return None


def now_iso():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def append(project_root, event):
    path = log_path(project_root)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as f:
        f.write(json.dumps(event, sort_keys=True) + "\n")
    return event


def record_selection(project_root, skill, session):
    """Called by the PreToolUse hook. ORES_SKILL_LEVEL is the attenuation
    axis: unset until the declared System filters the catalogue."""
    return append(project_root, {
        "kind": "selection",
        "id": uuid.uuid4().hex[:12],
        "ts": now_iso(),
        "session": session,
        "env": environment_label(project_root),
        "skill": skill,
        "offered": count_visible_skills(project_root),
        "level": os.environ.get("ORES_SKILL_LEVEL") or None,
    })


def read_events(project_root):
    path = log_path(project_root)
    events = []
    try:
        text = path.read_text(encoding="utf-8")
    except OSError:
        return events
    for line in text.splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            events.append(json.loads(line))
        except ValueError:
            continue
    return events


def parse_duration(spec):
    """'20m' / '2h' / '14d' -> timedelta, else None."""
    m = re.fullmatch(r"(\d+)([mhd])", spec.strip())
    if not m:
        return None
    n, unit = int(m.group(1)), m.group(2)
    return {"m": timedelta(minutes=n),
            "h": timedelta(hours=n),
            "d": timedelta(days=n)}[unit]


def within_window(events, since):
    if since is None:
        return events
    cutoff = (datetime.now(timezone.utc) - since).strftime("%Y-%m-%dT%H:%M:%SZ")
    return [e for e in events if e.get("ts", "") >= cutoff]


def summarise(events):
    """Selections, the corrections pointing at them, and the level split.

    A correction naming no selection still counts, so a correction recorded
    against a lost or trimmed selection is never silently dropped.
    """
    selections = {e["id"]: e for e in events
                  if e.get("kind") == "selection" and e.get("id")}
    corrections = [e for e in events if e.get("kind") == "correction"]

    corrected_ids = {c.get("selection") for c in corrections}
    by_level = Counter()
    corrected_by_level = Counter()
    for sid, sel in selections.items():
        level = sel.get("level") or "unattenuated"
        by_level[level] += 1
        if sid in corrected_ids:
            corrected_by_level[level] += 1

    return {
        "selections": selections,
        "corrections": corrections,
        "corrected": len(corrected_ids & set(selections)),
        "causes": Counter(c.get("cause") or "none" for c in corrections),
        "skills": Counter(s.get("skill") for s in selections.values()),
        "corrected_skills": Counter(
            selections[i]["skill"] for i in corrected_ids if i in selections),
        "by_level": by_level,
        "corrected_by_level": corrected_by_level,
        "offered": [s.get("offered") for s in selections.values()
                    if isinstance(s.get("offered"), int)],
    }


def cmd_report(argv, project_root):
    ap = argparse.ArgumentParser(
        prog="compass skills report",
        description="Report skill-selection quality over a window.")
    ap.add_argument("--since", default=DEFAULT_WINDOW,
                    help=f"Window, e.g. 20m/6h/14d (default: {DEFAULT_WINDOW}); "
                         "'all' for the whole log.")
    args = ap.parse_args(argv)

    since = None
    if args.since != "all":
        since = parse_duration(args.since)
        if since is None:
            print(f"❌  unrecognised window '{args.since}'; use 20m, 6h or 14d.",
                  file=sys.stderr)
            return 1

    events = within_window(read_events(project_root), since)
    s = summarise(events)
    n = len(s["selections"])
    c = len(s["corrections"])

    print(f"🧭 ores.compass — skill selection ({args.since})\n")
    if not n and not c:
        print("  No selections recorded yet.")
        print(f"  Log: {log_path(project_root)}")
        return 0

    rate = (s["corrected"] / n * 100) if n else 0.0
    print(f"  Selections:  {n}")
    print(f"  Corrections: {c}  ({rate:.0f}% of selections corrected)")
    if s["offered"]:
        lo, hi = min(s["offered"]), max(s["offered"])
        span = f"{lo}" if lo == hi else f"{lo}–{hi}"
        print(f"  Catalogue:   {span} skills visible")

    if s["causes"]:
        print("\n  Causes")
        for cause, count in s["causes"].most_common():
            print(f"    {count:4}  {cause}")

    print("\n  Level")
    for level, count in s["by_level"].most_common():
        bad = s["corrected_by_level"][level]
        pct = (bad / count * 100) if count else 0.0
        print(f"    {count:4}  {level}  ({bad} corrected, {pct:.0f}%)")

    if s["corrected_skills"]:
        print("\n  Most corrected")
        for skill, count in s["corrected_skills"].most_common(5):
            print(f"    {count:4}  {skill}")

    print("\n  Most chosen")
    for skill, count in s["skills"].most_common(5):
        print(f"    {count:4}  {skill}")
    return 0


def cmd_correct(argv, project_root):
    ap = argparse.ArgumentParser(
        prog="compass skills correct",
        description="Mark the last recorded selection as corrected.")
    ap.add_argument("--cause", required=True, choices=CAUSES,
                    help="Why the wrong skill was chosen.")
    ap.add_argument("--chose", default=None,
                    help="The skill that should have been chosen.")
    ap.add_argument("--selection", default=None,
                    help="Selection id to correct (default: the most recent).")
    args = ap.parse_args(argv)

    events = read_events(project_root)
    selections = [e for e in events if e.get("kind") == "selection"]
    if args.selection:
        target = next((e for e in selections
                       if e.get("id") == args.selection), None)
        if target is None:
            print(f"❌  no selection with id '{args.selection}'.",
                  file=sys.stderr)
            return 1
    elif selections:
        target = selections[-1]
    else:
        print("❌  nothing to correct: no selection recorded yet.",
              file=sys.stderr)
        return 1

    append(project_root, {
        "kind": "correction",
        "ts": now_iso(),
        "session": target.get("session"),
        "selection": target.get("id"),
        "cause": args.cause,
        "chose": args.chose,
    })
    instead = f" → {args.chose}" if args.chose else ""
    print(f"📝 corrected {target.get('skill')}{instead} "
          f"({args.cause}, selection {target.get('id')})")
    return 0


def run(argv, project_root):
    ap = argparse.ArgumentParser(
        prog="compass skills",
        description="The skill-selection instrument: report quality, record "
                    "a correction.")
    sub = ap.add_subparsers(dest="subcmd", required=True)
    sub.add_parser("report", add_help=False,
                   help="Selection quality over a window.")
    sub.add_parser("correct", add_help=False,
                   help="Mark the last selection as corrected.")
    if not argv:
        ap.print_help()
        return 1
    if argv[0] == "report":
        return cmd_report(argv[1:], project_root)
    if argv[0] == "correct":
        return cmd_correct(argv[1:], project_root)
    ap.parse_args(argv)
    return 1
