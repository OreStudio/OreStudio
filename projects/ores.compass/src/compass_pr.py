#!/usr/bin/env python3
"""compass pr — PR pillar: pull-request lifecycle verbs wrapping the gh CLI.

Subcommands:
  checks [pr]   CI status for a PR (default: the current branch's PR).
                --watch polls until checks settle; the exit code follows
                gh (0 green, non-zero on failing/pending checks) so
                runbooks can gate on it.
  record [pr]   Record a PR on its task doc: set #+pr: and add the row
                to the * PRs table. The task is found by matching
                #+branch: against the current branch (override with
                --task); idempotent when the PR is already recorded.

The review-round verbs (list, reply, resolve, pending) live in the
Review pillar: compass review --help.
"""

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path


def _cmd_checks(args, project_root):
    cmd = ["gh", "pr", "checks"]
    if args.pr:
        cmd.append(str(args.pr))
    if args.watch:
        cmd.append("--watch")
    if args.fail_fast:
        cmd.append("--fail-fast")
    if args.required:
        cmd.append("--required")
    if args.interval is not None:
        cmd += ["--interval", str(args.interval)]
    try:
        # Stream straight through so --watch updates live; gh's exit
        # code (0 green, non-zero otherwise) is the gating signal.
        return subprocess.run(cmd, cwd=str(project_root)).returncode
    except FileNotFoundError:
        print("❌ gh CLI not found on PATH.", file=sys.stderr)
        return 127


def _current_branch(project_root):
    p = subprocess.run(["git", "symbolic-ref", "--short", "HEAD"],
                       capture_output=True, text=True, cwd=str(project_root))
    return p.stdout.strip() if p.returncode == 0 else ""


def _find_task_doc(project_root, branch, task_ref):
    """Find the task doc to record on.

    With TASK_REF, match a task file by UUID or slug. Otherwise match
    #+branch: BRANCH across doc/agile task docs, preferring a STARTED
    task when several share the branch.
    """
    base = Path(project_root) / "doc" / "agile"
    candidates = []
    for path in base.rglob("task_*.org"):
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        if task_ref:
            if (task_ref.upper() in text[:200]
                    or path.stem == f"task_{task_ref}"):
                candidates.append((path, text))
            continue
        if re.search(rf"^#\+branch: {re.escape(branch)}\s*$", text, re.M):
            candidates.append((path, text))
    if len(candidates) > 1:
        started = [c for c in candidates
                   if re.search(r"^\| State\s*\|\s*STARTED", c[1], re.M)]
        if len(started) == 1:
            return started[0]
    return candidates[0] if candidates else (None, None)


def _cmd_record(args, project_root):
    # Resolve the PR: explicit number or the current branch's PR.
    sel = [str(args.pr)] if args.pr else []
    p = subprocess.run(
        ["gh", "pr", "view", *sel, "--json", "number,title,url"],
        capture_output=True, text=True, cwd=str(project_root))
    if p.returncode != 0:
        print(p.stderr.strip() or "❌ gh pr view failed.", file=sys.stderr)
        return p.returncode
    pr = json.loads(p.stdout)
    number, title, url = pr["number"], pr["title"], pr["url"]

    branch = _current_branch(project_root)
    path, text = _find_task_doc(project_root, branch, args.task)
    if path is None:
        print(f"❌ No task doc found for branch '{branch}' "
              "(set #+branch: on the task, or pass --task).",
              file=sys.stderr)
        return 1
    rel = path.relative_to(project_root)

    if f"/pull/{number}]" in text:
        print(f"✅ PR #{number} already recorded in {rel}")
        return 0

    text = re.sub(r"^#\+pr:.*$", f"#+pr: {number}", text,
                  count=1, flags=re.M)
    row = f"| [[{url}][#{number}]] | {title} |"
    empty = "| PR | Title |\n|----+-------|\n|    |       |"
    if empty in text:
        text = text.replace(empty, f"| PR | Title |\n|----+-------|\n{row}")
    else:
        text = text.replace("| PR | Title |\n|----+-------|",
                            f"| PR | Title |\n|----+-------|\n{row}")
    path.write_text(text, encoding="utf-8")
    print(f"✅ PR #{number} recorded in {rel}")
    print(f"   #+pr: {number}  +  * PRs row: {title}")
    return 0


def run(argv, project_root):
    """Entry point: compass pr <subcommand>."""
    ap = argparse.ArgumentParser(
        prog="compass pr",
        description="PR pillar: pull-request lifecycle verbs wrapping "
                    "the gh CLI.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    cp = sub.add_parser("checks", help="CI status for a PR")
    cp.add_argument("pr", nargs="?", default="",
                    help="PR number (default: the current branch's PR)")
    cp.add_argument("--watch", action="store_true",
                    help="Poll until all checks settle")
    cp.add_argument("--fail-fast", action="store_true",
                    help="With --watch, exit on the first failure")
    cp.add_argument("--required", action="store_true",
                    help="Only show checks marked required")
    cp.add_argument("--interval", type=int, default=None,
                    help="Polling interval in seconds for --watch "
                         "(default: gh's 10)")

    rp = sub.add_parser("record",
                        help="Record a PR on its task doc (#+pr: and the "
                             "* PRs table)")
    rp.add_argument("pr", nargs="?", type=int, default=0,
                    help="PR number (default: the current branch's PR)")
    rp.add_argument("--task", default="",
                    help="Task UUID or slug (default: match #+branch: "
                         "against the current branch)")

    args = ap.parse_args(argv)
    if args.subcmd == "checks" and args.interval is not None:
        if args.interval < 0:
            ap.error("argument --interval: must be a non-negative integer")
        if not args.watch:
            ap.error("argument --interval: only allowed with --watch")
    if args.subcmd == "checks":
        return _cmd_checks(args, project_root)
    if args.subcmd == "record":
        return _cmd_record(args, project_root)
    return 1
