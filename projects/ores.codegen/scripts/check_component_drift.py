#!/usr/bin/env python3
"""
Regenerate every codegen-eligible model for a set of components and fail if
the working tree ends up dirty -- i.e. someone hand-edited a generated file
instead of its .org model source, or a model was changed without running
`compass codegen regenerate` afterwards.

Deliberately narrow-scoped by default (--components refdata): rolling this
check out to every component at once would surface a large pre-existing
drift backlog unrelated to whatever change triggered CI. Extend --components
as each component is brought to a verified zero-diff state (see
doc/agile/versions/v0/sprint_24/entity-classification-drift-baseline/), the
same way ores.refdata was for this check to be trustworthy here.

Usage:
  check_component_drift.py --components refdata
  check_component_drift.py --components refdata,assets.core
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument(
        "--components",
        required=True,
        metavar="NAME[,NAME...]",
        help="Comma-separated component slugs to regenerate (e.g. refdata)",
    )
    ap.add_argument("--address", default="ores", metavar="ADDRESS")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    configure(verbose=args.verbose)

    for component in (c.strip() for c in args.components.split(",") if c.strip()):
        print(f"Regenerating component {component!r} at address {args.address!r}...")
        regen_args = SimpleNamespace(
            component=component, all=False, address=args.address,
            entity=None, dry_run=False,
        )
        rc = cmd_regenerate(regen_args, CODEGEN_DIR)
        if rc != 0:
            print(f"codegen regenerate failed for component {component!r}", file=sys.stderr)
            return rc

    diff = subprocess.run(["git", "diff", "--exit-code"], cwd=REPO_ROOT, check=False)
    if diff.returncode != 0:
        print(
            "\nGenerated output does not match what's checked in -- a "
            "generated file was hand-edited, or its .org model changed "
            "without running `compass codegen regenerate` afterwards. "
            "Run the regenerate command locally and commit the result.",
            file=sys.stderr,
        )
        return 1

    print("No drift: regenerated output matches the checked-in tree.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
