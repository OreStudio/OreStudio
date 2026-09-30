#!/usr/bin/env python3
"""Decide which heavy pull-request checks a diff needs.

The workflow calls this once per pull request and gates the compile-and-test
job on what it prints. The rules live in
``codegen.change_classification``; this file is the command around them: it
reads the changed paths, or asks git for them, and writes the decision both to
the console (for the log) and to the file GitHub reads outputs from.

A diff that cannot be determined -- no merge base, an unavailable base -- asks
for everything. A gate that cannot see the change must not stand in front of
the break the change hides.
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.change_classification import classify  # noqa: E402


def changed_paths(base: str) -> list[str] | None:
    """The changed paths against ``base``, or None when git cannot say.

    The merge base is what a pull request's diff means; without one the
    comparison against the base's tip is the best available answer, and if
    even that fails the caller is told nothing rather than something wrong.
    """
    for args in (
        ["git", "diff", "--name-only", "--merge-base", base, "HEAD"],
        ["git", "diff", "--name-only", base, "HEAD"],
    ):
        result = subprocess.run(
            args, cwd=REPO_ROOT, capture_output=True, text=True, check=False
        )
        if result.returncode == 0:
            return [line for line in result.stdout.splitlines() if line.strip()]
    return None


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--base",
        default="origin/main",
        help="the ref the change is compared against (default: origin/main)",
    )
    parser.add_argument(
        "--paths",
        nargs="*",
        default=None,
        help="changed paths to classify instead of asking git",
    )
    parser.add_argument(
        "--github-output",
        default=None,
        help="the file GitHub reads step outputs from; append the flags to it",
    )
    args = parser.parse_args()

    paths = args.paths if args.paths is not None else changed_paths(args.base)
    if paths is None:
        print(
            f"::warning::could not diff against {args.base}; running every heavy check"
        )
        paths = ["unknown/diff"]

    decision = classify(paths)
    for reason in decision.reasons:
        print(f"  {reason}")

    outputs = decision.as_github_outputs()
    print(
        "heavy checks: "
        f"cpp={outputs['cpp']} db={outputs['db']} services={outputs['services']}"
    )
    if args.github_output:
        with open(args.github_output, "a", encoding="utf-8") as handle:
            for key, value in outputs.items():
                handle.write(f"{key}={value}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
