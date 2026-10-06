#!/usr/bin/env python3
"""Check that every prototype is in the inventory, and that no prototype is React.

Why this exists
---------------

Two prototype mechanisms grew. The first was plain static pages under
doc/prototypes/, listed in doc/prototypes/index.org. The second was React
routes inside ores.web, listed only by the hub at /prototype. The React form
needed the application's shell, its build and its types, so a prototype could
not open without running the app, could not deploy with the documentation, it
drifted with the product, and the two inventories disagreed.

On 2026-10-06 the project kept one way: plain HTML, CSS and JavaScript under
doc/prototypes/<name>/, listed in doc/prototypes/index.org and published by
the site build. This gate keeps the second way from growing back.

How it checks
-------------

- Every doc/prototypes/<name>/index.html has a row in doc/prototypes/index.org.
- Every row in the inventory names a real prototype directory.
- Every prototype directory is published by an ores-deploy-web-app call in
  projects/ores.lisp/src/ores-build-site.el.
- The React prototype tree and its route table are gone.

Usage
-----

    python3 build/scripts/check_prototype_inventory.py [--root .]
"""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

# A row opens its prototype with [[file:<name>/][...]].
INVENTORY_RE = re.compile(r"\[\[file:([^/\]]+)/")

# The site build publishes each prototype with one call naming its directory.
DEPLOY_RE = re.compile(r"\./doc/prototypes/([a-z0-9-]+)")

# The React prototype tree is retired; its presence is the drift this gate exists
# to catch.
REACT_PROTOTYPE_DIRS = (
    "projects/ores.web/packages/web/src/prototype",
    "projects/ores.web/packages/web/src/pages/prototype",
)


def prototype_dirs(root: pathlib.Path) -> set[str]:
    """Every directory under doc/prototypes/ that holds an index.html."""
    base = root / "doc" / "prototypes"
    if not base.is_dir():
        return set()
    return {path.parent.name for path in base.glob("*/index.html")}


def listed(root: pathlib.Path) -> set[str]:
    """Every prototype name the inventory links."""
    index = root / "doc" / "prototypes" / "index.org"
    if not index.is_file():
        return set()
    return set(INVENTORY_RE.findall(index.read_text(encoding="utf-8")))


def published(root: pathlib.Path) -> set[str]:
    """Every prototype name the site build publishes."""
    site = root / "projects" / "ores.lisp" / "src" / "ores-build-site.el"
    if not site.is_file():
        return set()
    return set(DEPLOY_RE.findall(site.read_text(encoding="utf-8")))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=".", help="repository root")
    args = parser.parse_args()
    root = pathlib.Path(args.root).resolve()

    present = prototype_dirs(root)
    inventory = listed(root)
    deployed = published(root)
    problems: list[str] = []

    for name in sorted(present - inventory):
        problems.append(
            f"doc/prototypes/{name}/index.html is not in doc/prototypes/index.org"
        )
    for name in sorted(inventory - present):
        problems.append(
            f"doc/prototypes/index.org lists {name}, but "
            f"doc/prototypes/{name}/index.html does not exist"
        )
    for name in sorted(present - deployed):
        problems.append(
            f"doc/prototypes/{name}/ is not published: add an ores-deploy-web-app "
            "call in projects/ores.lisp/src/ores-build-site.el"
        )
    for name in sorted(deployed - present):
        problems.append(
            f"ores-build-site.el publishes doc/prototypes/{name}/, which does not exist"
        )
    for relative in REACT_PROTOTYPE_DIRS:
        if (root / relative).exists():
            problems.append(
                f"{relative} still exists: React is for the product, not for prototypes"
            )

    if problems:
        print("Prototype inventory is not consistent:", file=sys.stderr)
        for problem in problems:
            print(f"  - {problem}", file=sys.stderr)
        return 1

    print(
        f"Prototype inventory: {len(present)} prototype(s), all listed and published; "
        "no React prototype tree."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
