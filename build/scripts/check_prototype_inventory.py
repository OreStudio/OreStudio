#!/usr/bin/env python3
"""Check that every prototype is grouped, listed, reviewed and published.

Why this exists
---------------

Two prototype mechanisms grew. The first was plain static pages under
doc/prototypes/, listed in doc/prototypes/index.org. The second was React
routes inside ores.web, listed only by the hub at /prototype. The React form
needed the application's shell, its build and its types, so a prototype could
not open without running the app, could not deploy with the documentation, it
drifted with the product, and the two inventories disagreed.

On 2026-10-06 the project kept one way: plain HTML, CSS and JavaScript under
doc/prototypes/<name>/, listed in doc/prototypes/index.org.

That left one flat list of every prototype, so nothing could be found. On
2026-10-08 the prototypes were grouped by the journey topic they answer to:
one top page naming the groups, one page per group listing its prototypes, and
one folder per group holding them. A Gemini review, where one exists, sits at
doc/prototypes/<group>/<name>/review/, beside the prototype it reviews.

A prototype is a design record, so it is published exactly as it was written
and carries nothing the site adds: a page that the site decorates is no longer
the page that was agreed. The group page is where a reader is told what a
prototype argues and whether anyone has reviewed it, and its review cell says
none for a prototype nobody has reviewed, so the unreviewed set is one grep.

This gate keeps the second way from growing back, and keeps the group pages the
only inventory a reader can trust.

How it checks
-------------

- Every doc/prototypes/<group>/<name>/index.html has a row on
  doc/prototypes/<group>/index.org.
- Every row on a group page names a real prototype directory.
- A row whose prototype has a review links it, and a row whose prototype has
  none says none.
- Every review directory holds an index.html.
- Every group that holds a prototype has a group page, and the top page links
  it.
- The site build discovers the prototypes rather than naming them, so a new
  prototype needs no change there.
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

# A group page opens a prototype with [[file:<name>/][...]] and a review with
# [[file:<name>/review/][...]].
PROTOTYPE_RE = re.compile(r"\[\[file:([a-z0-9][a-z0-9-]*)/")

# The top page opens a group page with [[file:<group>/][...]].
GROUP_RE = re.compile(r"\[\[file:([a-z0-9][a-z0-9_]*)/")

# The site build deploys every prototype it finds under the group folders.
DISCOVERY_CALL = "ores-deploy-prototypes"

# A prototype nobody has reviewed says so in the review cell of its row.
NO_REVIEW = "none"

# The React prototype tree is retired; its presence is the drift this gate
# exists to catch.
REACT_PROTOTYPE_DIRS = (
    "projects/ores.web/packages/web/src/prototype",
    "projects/ores.web/packages/web/src/pages/prototype",
)


def prototypes(root: pathlib.Path) -> set[tuple[str, str]]:
    """Every (group, name) with a doc/prototypes/<group>/<name>/index.html."""
    base = root / "doc" / "prototypes"
    if not base.is_dir():
        return set()
    return {(path.parent.parent.name, path.parent.name)
            for path in base.glob("*/*/index.html")}


def reviews(root: pathlib.Path) -> set[tuple[str, str]]:
    """Every (group, name) holding a review directory."""
    base = root / "doc" / "prototypes"
    if not base.is_dir():
        return set()
    return {(path.parent.parent.name, path.parent.name)
            for path in base.glob("*/*/review")}


def group_pages(root: pathlib.Path) -> set[str]:
    """Every group that has a page."""
    base = root / "doc" / "prototypes"
    if not base.is_dir():
        return set()
    return {path.parent.name for path in base.glob("*/index.org")}


def _read(path: pathlib.Path) -> str:
    return path.read_text(encoding="utf-8") if path.is_file() else ""


def listed(root: pathlib.Path) -> set[tuple[str, str]]:
    """Every (group, prototype) a group page lists."""
    found: set[tuple[str, str]] = set()
    for group in group_pages(root):
        page = root / "doc" / "prototypes" / group / "index.org"
        found |= {(group, name) for name in PROTOTYPE_RE.findall(_read(page))}
    return found


def rows(root: pathlib.Path) -> dict[tuple[str, str], str]:
    """The row a group page writes for each prototype, keyed by (group, name)."""
    found: dict[tuple[str, str], str] = {}
    for group in group_pages(root):
        page = root / "doc" / "prototypes" / group / "index.org"
        for line in _read(page).splitlines():
            for name in PROTOTYPE_RE.findall(line):
                found[(group, name)] = line
    return found


def linked_groups(root: pathlib.Path) -> set[str]:
    """Every group the top page links."""
    return set(GROUP_RE.findall(_read(root / "doc" / "prototypes" / "index.org")))


def site_build_discovers(root: pathlib.Path) -> bool:
    """Whether the site build deploys the prototypes it finds."""
    site = root / "projects" / "ores.lisp" / "src" / "ores-build-site.el"
    return DISCOVERY_CALL in _read(site)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=".", help="repository root")
    args = parser.parse_args()
    root = pathlib.Path(args.root).resolve()

    present = prototypes(root)
    pages = group_pages(root)
    listed_rows = listed(root)
    reviewed = reviews(root)
    page_rows = rows(root)
    groups_linked = linked_groups(root)
    problems: list[str] = []

    for group, name in sorted(present - listed_rows):
        problems.append(
            f"doc/prototypes/{group}/{name}/index.html is not listed on "
            f"doc/prototypes/{group}/index.org"
        )
    for group, name in sorted(listed_rows - present):
        problems.append(
            f"doc/prototypes/{group}/index.org lists {name}, but "
            f"doc/prototypes/{group}/{name}/index.html does not exist"
        )
    for group in sorted({g for g, _ in present} - pages):
        problems.append(
            f"doc/prototypes/{group}/ holds prototypes but has no index.org: "
            "the group needs a page"
        )
    for group in sorted(pages - groups_linked):
        problems.append(
            f"doc/prototypes/{group}/index.org is not linked from "
            "doc/prototypes/index.org: the top page must name every group"
        )
    for group in sorted(groups_linked - pages):
        problems.append(
            f"doc/prototypes/index.org links the group {group}, but "
            f"doc/prototypes/{group}/index.org does not exist"
        )
    for group, name in sorted(reviewed):
        review_index = (root / "doc" / "prototypes" / group / name
                        / "review" / "index.html")
        if not review_index.is_file():
            problems.append(
                f"doc/prototypes/{group}/{name}/review/ has no index.html: a "
                "review lists its screens on one page"
            )
    # The review cell of a row is the only place a reader is told whether a
    # prototype has been reviewed, and the only place the unreviewed set can be
    # counted, so it must agree with the review directory.
    for group, name in sorted(present):
        row = page_rows.get((group, name))
        if row is None:
            continue
        if (group, name) in reviewed:
            if f"[[file:{name}/review/" not in row:
                problems.append(
                    f"doc/prototypes/{group}/index.org lists {name} without its "
                    "review, though the review directory exists"
                )
        elif not row.rstrip().endswith(f"| {NO_REVIEW} |"):
            problems.append(
                f"doc/prototypes/{group}/index.org lists {name} without a review, "
                f"so its review cell must say {NO_REVIEW}: an unreviewed prototype "
                "is found by that row"
            )
    if not site_build_discovers(root):
        problems.append(
            f"the site build does not call {DISCOVERY_CALL}: the prototypes "
            "must be deployed where they sit, with no list to keep in step"
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
        f"Prototype inventory: {len(present)} prototype(s) in {len(pages)} "
        f"group(s), all listed and published; {len(reviewed)} Gemini review(s), "
        f"{len(present) - len(reviewed)} unreviewed; no React prototype tree."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
