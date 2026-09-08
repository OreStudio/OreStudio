#!/usr/bin/env python3
"""
Regenerate the codegen-eligible models of drift-free components and fail if
the working tree ends up dirty -- someone hand-edited a generated file
instead of its .org model source, or a model changed without running
`compass codegen regenerate` afterwards.

The check runs in exactly two modes:

  --all           regenerate every component in the known-drift-free
                  registry below; the local pr-raise gate covers this set
  --component X   regenerate one named component, by catalogue slug

An ad hoc multi-component list is never a valid invocation: a component
whose committed tree predates a template receives the newer per-entity
families as untracked files on regeneration, and git diff cannot see
untracked files. The check fails when regeneration materializes
untracked files that were not already in the tree, so a component joins
the registry only when its regeneration leaves the tree fully clean.
See the regen-byproduct-hygiene memory in doc/llm/memory/.

Usage:
  check_component_drift.py --all
  check_component_drift.py --component refdata
"""
from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path
from types import SimpleNamespace

REPO_ROOT = Path(__file__).resolve().parents[3]
CODEGEN_DIR = REPO_ROOT / "projects" / "ores.codegen"
sys.path.insert(0, str(CODEGEN_DIR / "src"))

from codegen.generate import cmd_regenerate  # noqa: E402
from codegen.logging_config import configure  # noqa: E402

# Components verified to regenerate byte-identical to their committed
# tree, with no untracked materialization. --all checks exactly this
# set, the drift gate the pr-raise skill runs. A component joins only
# when codegen-fix-drift step 7 verifies its regeneration leaves the
# tree fully clean.
KNOWN_DRIFT_FREE = (
    "refdata",
    "reporting",
    "marketdata",
    "compute-cpp",
    "iam",
    "iam-cpp",
    "synthetic",
)


def _untracked_files() -> set:
    ls = subprocess.run(
        ["git", "ls-files", "--others", "--exclude-standard"],
        cwd=REPO_ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    if ls.returncode != 0:
        print(f"git ls-files --others failed:\n{ls.stderr}", file=sys.stderr)
        sys.exit(ls.returncode)
    return {line for line in ls.stdout.splitlines() if line}


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    modes = ap.add_mutually_exclusive_group(required=True)
    modes.add_argument(
        "--all",
        action="store_true",
        help="regenerate every known-drift-free component "
        f"({', '.join(KNOWN_DRIFT_FREE)})",
    )
    modes.add_argument(
        "--component",
        metavar="NAME",
        help="regenerate one component by catalogue slug (e.g. refdata)",
    )
    ap.add_argument("--address", default="ores", metavar="ADDRESS")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    configure(verbose=args.verbose)

    components = list(KNOWN_DRIFT_FREE) if args.all else [args.component]
    untracked_before = _untracked_files()

    for component in components:
        print(f"Regenerating component {component!r} at address {args.address!r}...")
        regen_args = SimpleNamespace(
            component=component, all=False, address=args.address,
            entity=None, dry_run=False,
        )
        rc = cmd_regenerate(regen_args, CODEGEN_DIR)
        if rc != 0:
            print(f"codegen regenerate failed for component {component!r}",
                  file=sys.stderr)
            materialized = sorted(_untracked_files() - untracked_before)
            if materialized:
                print("The failed run materialized untracked files "
                      "(sweep these before retrying):", file=sys.stderr)
                for path in materialized:
                    print(f"  {path}", file=sys.stderr)
            return rc

    failures = 0
    diff = subprocess.run(["git", "diff"], cwd=REPO_ROOT, check=False,
                          capture_output=True, text=True)
    if diff.stdout:
        stat = subprocess.run(["git", "diff", "--stat"], cwd=REPO_ROOT, check=False,
                              capture_output=True, text=True)
        print(f"\n--- drifted file(s) ---\n{stat.stdout}", file=sys.stderr)
        print(f"--- unified diff ---\n{diff.stdout}", file=sys.stderr)
        print(
            "Generated output does not match what's checked in -- a "
            "generated file was hand-edited, or its .org model changed "
            "without running `compass codegen regenerate` afterwards. "
            "Run the regenerate command locally and commit the result.",
            file=sys.stderr,
        )
        failures += 1

    materialized = sorted(_untracked_files() - untracked_before)
    if materialized:
        print("\n--- untracked file(s) materialized by regeneration ---",
              file=sys.stderr)
        for path in materialized:
            print(f"  {path}", file=sys.stderr)
        print(
            "Regeneration materialized untracked files: the component's "
            "committed tree predates a template family, or a generated "
            "file was never committed. git diff cannot see untracked "
            "files, so the check fails on them explicitly. Commit the "
            "files or bring the component to a fully committed state "
            "before it can pass.",
            file=sys.stderr,
        )
        failures += 1

    if failures:
        return 1

    print("No drift: regenerated output matches the checked-in tree.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
