#!/usr/bin/env python3
"""Prove the registrar merges kept everything main serves.

The rebase restructured the registrars onto `step_routes` / `serve_step_commands`
while main was adding subjects and handlers to the same files. A resolution that
keeps the new shape but drops one of main's additions is silent: the file
compiles, the gates pass, and one subject simply stops being served.

So this compares the merged file against main's own version of it and reports
every identifier main has that the merged file does not. Two identifiers are
expected to be absent, because our shape replaces them outright:
`ensure_step_stream` and `subscribe_step_command`. They are listed as REPLACED
rather than filtered away, so a reviewer sees the substitution instead of having
to trust that it was accounted for. Anything else missing is a failure.

    python3 .audit/check_registrar_merge_completeness.py
    python3 .audit/check_registrar_merge_completeness.py --base origin/main
"""
from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent

# Every file the rebase had to resolve on the step-command transport.
FILES = (
    "projects/ores.analytics/core/src/messaging/registrar.cpp",
    "projects/ores.assets/core/src/messaging/registrar.cpp",
    "projects/ores.compute/core/src/messaging/registrar.cpp",
    "projects/ores.dq/core/src/messaging/registrar.cpp",
    "projects/ores.iam/core/src/messaging/registrar.cpp",
    "projects/ores.inbox/core/src/messaging/registrar.cpp",
    "projects/ores.marketdata/core/src/messaging/registrar.cpp",
    "projects/ores.ore/service/src/messaging/registrar.cpp",
    "projects/ores.refdata/core/src/messaging/registrar.cpp",
    "projects/ores.refdata/service/src/messaging/book_proposal_registrar.cpp",
    "projects/ores.reporting/core/src/messaging/registrar.cpp",
    "projects/ores.workflow/core/src/messaging/identity_step_handler.cpp",
)

# What our shape replaces, so their absence is the point rather than a loss.
REPLACED = {"ensure_step_stream", "subscribe_step_command"}

# Identifiers that name something a service serves or wires: a subject constant,
# a registrar call, or a handler method call.
IDENT = re.compile(
    r"\b(?:"
    r"register_\w+"                       # generated per-entity registrars
    r"|\w+_subject"                       # subject constants
    r"|ensure_step_stream|subscribe_step_command|serve_step_commands"
    r"|[\w:]*Handler\w*|handle\w*"        # handler types and entry points
    r")\b"
)

SUBJECT_LITERAL = re.compile(r'"([a-z0-9_.]+\.v1\.[a-z0-9_.]+)"')


def identifiers(text: str) -> set[str]:
    return set(IDENT.findall(text)) | set(SUBJECT_LITERAL.findall(text))


def show(ref: str, path: str) -> str | None:
    r = subprocess.run(["git", "show", f"{ref}:{path}"],
                       cwd=REPO, capture_output=True, text=True)
    return r.stdout if r.returncode == 0 else None


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--base", default="origin/main",
                    help="the ref holding main's side of the merge")
    args = ap.parse_args()

    failed, checked = [], 0
    for path in FILES:
        base_text = show(args.base, path)
        local = REPO / path
        if base_text is None:
            print(f"skip   {path} (not in {args.base})")
            continue
        if not local.is_file():
            print(f"FAIL   {path} (gone from the tree)")
            failed.append(path)
            continue
        checked += 1
        missing = identifiers(base_text) - identifiers(local.read_text())
        replaced = sorted(missing & REPLACED)
        lost = sorted(missing - REPLACED)
        line = f"ok     {path}" if not lost else f"FAIL   {path}"
        print(line)
        for name in replaced:
            print(f"         REPLACED by our shape: {name}")
        for name in lost:
            print(f"         MISSING, main serves it and we do not: {name}")
        if lost:
            failed.append(path)

    print()
    if failed:
        print(f"FAIL: {len(failed)} of {checked} file(s) lost something main has.")
        return 1
    print(f"ok: all {checked} merged registrar file(s) still name everything "
          f"{args.base} serves, apart from the two our shape replaces.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
