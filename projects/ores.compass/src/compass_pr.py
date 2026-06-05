#!/usr/bin/env python3
"""compass pr — PR pillar: pull-request lifecycle verbs wrapping the gh CLI.

Subcommands:
  checks [pr]   CI status for a PR (default: the current branch's PR).
                --watch polls until checks settle; the exit code follows
                gh (0 green, non-zero on failing/pending checks) so
                runbooks can gate on it.

The review-round verbs (list, reply, resolve, pending) live in the
Review pillar: compass review --help.
"""

import argparse
import subprocess
import sys


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
    if args.interval:
        cmd += ["--interval", str(args.interval)]
    try:
        # Stream straight through so --watch updates live; gh's exit
        # code (0 green, non-zero otherwise) is the gating signal.
        return subprocess.run(cmd, cwd=str(project_root)).returncode
    except FileNotFoundError:
        print("❌ gh CLI not found on PATH.", file=sys.stderr)
        return 127


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
    cp.add_argument("--interval", type=int, default=0,
                    help="Polling interval in seconds for --watch "
                         "(default: gh's 10)")

    args = ap.parse_args(argv)
    if args.subcmd == "checks":
        return _cmd_checks(args, project_root)
    return 1
