# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
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
"""Write the renderings of each ORE value as a file the value sets read.

The renderings live in ORE's reader today, in the tables that convert a spelling
into the form the store holds. This writes the same knowledge as data -- one
entry per canonical form, listing the renderings that mean it -- so the value
sets read a file rather than a compiled table.

It is a step, not the end: the reader's tables still state the same thing, so
--check compares the file with them and fails when the two disagree. When the
reader stops converting, the file is authored rather than written, and the
comparison goes with the tables.

    generate_value_renderings.py          write the file
    generate_value_renderings.py --check  fail if the committed file differs
"""

import argparse
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ore_aliases  # noqa: E402

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "ore_renderings.json")

HEADER = (
    "The renderings ORE accepts for each value, and the form a configuration "
    "holds. One entry per table; each entry maps the canonical form to every "
    "rendering that means it, the canonical form included. Written by "
    "generate_value_renderings.py from ORE's reader."
)


def data():
    """The renderings, by table and canonical form, in the order ORE's reader states."""
    tables = {}
    for name, pairs in ore_aliases.reader_tables().items():
        groups = {}
        order = []
        for spelling, canonical in pairs:
            if canonical not in groups:
                groups[canonical] = [canonical]
                order.append(canonical)
            if spelling != canonical and spelling not in groups[canonical]:
                groups[canonical].append(spelling)
        tables[name] = {canonical: groups[canonical] for canonical in order}
    return {"description": HEADER, "tables": tables}


def render(document):
    return json.dumps(document, indent=2, sort_keys=False, ensure_ascii=False) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if the committed file differs from ORE's reader")
    args = parser.parse_args()
    content = render(data())
    if args.check:
        current = open(OUT, encoding="utf-8").read() if os.path.exists(OUT) else ""
        if current != content:
            print(f"{OUT} differs from ORE's reader; regenerate it.", file=sys.stderr)
            return 1
        print(f"{sum(len(v) for v in data()['tables'].values())} canonical forms match "
              "ORE's reader.")
        return 0
    with open(OUT, "w", encoding="utf-8") as handle:
        handle.write(content)
    print(f"Wrote {OUT}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
