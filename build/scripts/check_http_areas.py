#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Every declared HTTP area is one the deployment publishes.

An entity model may state =#+http_area: <area>= to publish its routes under an
area instead of its component's name. The area is part of a URL, so a typo in one
is a URL that exists and belongs to no area at all: this check refuses that.

The published list is the one the inventory is classified against, so there is one
list of areas in the tree and not two.
"""

import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "build/scripts"))

from route_classification import AREAS, NO_AREA  # noqa: E402

DECLARATION = re.compile(r"^#\+http_area:\s*(\S+)\s*$", re.M)


def declarations():
    """Every =#+http_area:= in the tree, with the file that states it."""
    for path in sorted(ROOT.glob("projects/*/modeling/*.org")):
        text = path.read_text(encoding="utf-8", errors="replace")
        for match in DECLARATION.finditer(text):
            yield path.relative_to(ROOT).as_posix(), match.group(1)


def main():
    published = set(AREAS) - {NO_AREA}
    declared = list(declarations())
    unknown = [(path, area) for path, area in declared if area not in published]
    if unknown:
        for path, area in unknown:
            print(f"{path}: declares http_area {area!r}, which is not a published area")
        print("published areas: " + ", ".join(sorted(published)))
        print("add the area to route_classification.AREAS, or use one of the above")
        return 1
    print(f"{len(declared)} declared HTTP area(s), all published")
    return 0


if __name__ == "__main__":
    sys.exit(main())
