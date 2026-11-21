#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil; python-indent-offset: 4 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""Every #+audience: keyword says user, and appears once in a page's frontmatter.

A page that says #+audience: user is loaded into the knowledge base and shown to
every tenant. The keyword is opt-in, so a misspelt value or a second, contrary
line must fail loudly rather than load a page nobody meant to publish, or drop
one somebody did.

Exit 0 when every declaration is well formed, 1 naming each that is not.
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ALLOWED = {"user"}
KEYWORD = re.compile(r"^#\+audience:(.*)$", re.M | re.I)


def problems(path: Path) -> list[str]:
    """What is wrong with the page's declarations, if anything."""
    found = KEYWORD.findall(path.read_text(encoding="utf-8", errors="replace"))
    out = []
    if len(found) > 1:
        out.append(f"declares an audience {len(found)} times")
    for value in found:
        if value.strip() not in ALLOWED:
            out.append(f"audience {value.strip()!r} is not one of {sorted(ALLOWED)}")
    return out


def main() -> int:
    failures = 0
    for path in sorted((ROOT / "doc").rglob("*.org")):
        for problem in problems(path):
            print(f"{path.relative_to(ROOT)}: {problem}")
            failures += 1
    if failures:
        print(f"{failures} malformed audience declaration(s)")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
