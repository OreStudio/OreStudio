#!/usr/bin/env python3
"""Fail when the XML code moved and the benchmark history did not.

Requirement: a change to the XML layer is measured, and the measurement is
recorded. The gate is deliberately narrow. It looks at the files that change
what the benchmark measures, which is the XML sources and the benchmark's own
harness, and at nothing else. A documentation edit, a test, or a change
elsewhere in the tree never asks for a measurement.

A row counts when its commit is the last commit that touched those files, or a
descendant of it. That way a later commit that does not touch them, such as a
record update or a docs fix, does not invalidate a measurement already taken.
"""

import os
import subprocess
import sys

WATCHED = (
    "projects/ores.ore/core/src/xml",
    "projects/ores.ore/core/include/ores.ore.core/xml",
    "benchmarks/xml/bench.cpp",
    "benchmarks/xml/run.sh",
    "benchmarks/xml/inputs.tsv",
)

HISTORY = "benchmarks/xml/history.tsv"


def git(*args):
    return subprocess.run(
        ("git",) + args, capture_output=True, text=True, check=False
    ).stdout.strip()


def measures_the_same_files(commit, last):
    """True when a recorded commit holds the same watched files as now.

    Content, not ancestry. A merge commit on main introduces the branch's
    changes, so the recorded branch commit is an ancestor of it rather than a
    descendant, and an ancestry test would fail on main after every merge. Two
    commits with the same bytes in the watched paths describe the same
    measurement.
    """
    return (
        subprocess.run(
            ("git", "diff", "--quiet", last, commit, "--") + WATCHED,
            capture_output=True,
            check=False,
        ).returncode
        == 0
    )


def main():
    root = git("rev-parse", "--show-toplevel")
    if not root:
        print("not a git checkout; nothing to check")
        return 0
    os.chdir(root)

    last = git("log", "-1", "--format=%H", "--", *WATCHED)
    if not last:
        print("no commit has touched the XML code; nothing to measure")
        return 0

    history = os.path.join(root, HISTORY)
    if not os.path.exists(history):
        print(f"{HISTORY} does not exist, so nothing is recorded")
        print(f"Run ./benchmarks/xml/run.sh and commit the result.")
        return 1

    recorded = set()
    with open(history, encoding="utf-8") as handle:
        for line in handle:
            fields = line.rstrip("\n").split("\t")
            if len(fields) >= 6 and fields[0] != "mode":
                recorded.add(fields[5])

    if any(measures_the_same_files(commit, last) for commit in recorded):
        return 0

    print("The XML code changed and no measurement was recorded after it.")
    print(f"  last commit touching the measured files: {last[:12]}")
    print(f"  newest recorded commit:                  {sorted(recorded)[-1][:12]}")
    print("")
    print("Run ./benchmarks/xml/run.sh and commit benchmarks/xml/history.tsv.")
    print("If the change cannot move any number, say so on the PR and skip this")
    print("check with --only, and record the reason there.")
    return 1


if __name__ == "__main__":
    sys.exit(main())
