#!/usr/bin/env python3
# -*- coding: utf-8 -*-
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
"""Extract the index families ORE defines in code.

ORE's =OREData/ored/utilities/indexparser.cpp= holds two hardcoded maps: the
overnight indices it builds when no convention defines the index, and the Ibor
indices, each under the name ORE uses for the index's family. Every other index
ORE knows arrives through a conventions file, so these two maps are the whole
of what a tenant holds without one.

One row per family: the name ORE looks the family up by, whether it is an
overnight or an Ibor index, and the QuantLib or QuantExt class ORE builds it
from. The class is what says how the index is fixed, so it is carried through
to the catalogue run that reads the physical fields.

The engine's source is not vendored, so this takes the checkout as an argument
and the result is committed beside it:

    ./tools/ore_conventions/extract_builtin_indices.py <ORE source dir>
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

# The declaration that opens each map, and the kind it holds.
MAPS = (
    ("overnight", "shared_ptr<OvernightIndex>> onIndices"),
    ("ibor", "shared_ptr<IborIndexParser>> iborIndices"),
)

# {"EUR-EURIBOR", QuantLib::ext::make_shared<IborIndexParserWithPeriod<Euribor>>()},
# The tenor an Ibor family is probed at. ORE's Ibor map holds the family stem,
# and a few families branch on the tenor: Euribor rolls Following and no end of
# month for days and weeks, ModifiedFollowing and end of month for months and
# years. A month tenor is the branch every desk quotes, and the branch the seed
# states, so the probe states one.
PROBE_TENOR = "-3M"

ENTRY = re.compile(r'\{"([A-Za-z0-9 ./_+-]+)"\s*,\s*(.+)\}')
CLASS = re.compile(r"(\w+)")


def map_body(text: str, marker: str) -> str:
    """The initialiser of the map @p marker opens, up to its closing brace."""
    start = text.find(marker)
    if start < 0:
        raise SystemExit(f"indexparser.cpp: no map declared by {marker!r}")
    end = text.find("};", start)
    if end < 0:
        raise SystemExit(f"indexparser.cpp: the map at {marker!r} never closes")
    return text[start:end]


def entries(body: str) -> list[tuple[str, str]]:
    """Every (name, class) pair in one map's initialiser."""
    found = []
    for line in body.splitlines():
        if not line.strip().startswith("{"):
            continue
        m = ENTRY.search(line)
        if not m:
            continue
        # The class is the last name inside the make_shared's argument list,
        # whatever wrapper or namespace ORE puts around it.
        names = CLASS.findall(m.group(2))
        if not names:
            raise SystemExit(f"indexparser.cpp: no class in {line.strip()}")
        found.append((m.group(1), names[-1]))
    return found


def extract(source: Path) -> list[tuple[str, str, str]]:
    parser = source / "OREData" / "ored" / "utilities" / "indexparser.cpp"
    try:
        text = parser.read_text(encoding="utf-8")
    except OSError as exc:
        raise SystemExit(f"cannot read {parser}: {exc}")
    rows = []
    for kind, marker in MAPS:
        body = map_body(text, marker)
        got = entries(body)
        if not got:
            raise SystemExit(f"indexparser.cpp: the {kind} map reads as empty")
        rows.extend((name, kind, klass) for name, klass in got)
    # A family may be both: ORE builds USD-SOFR as the overnight index and
    # USD-SOFR-3M as a term rate from the Ibor map. The kind is part of the
    # identity, so one family yields two rows.
    pairs = [(r[0], r[1]) for r in rows]
    duplicates = sorted({p for p in pairs if pairs.count(p) > 1})
    if duplicates:
        raise SystemExit("a family is declared twice as one kind: " +
                         ", ".join(f"{n} ({k})" for n, k in duplicates))
    return sorted(rows)


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print("usage: extract_builtin_indices.py <ORE source dir>", file=sys.stderr)
        return 2
    rows = extract(Path(argv[1]))
    print("# The index families ORE defines in code, mined from")
    print("# OREData/ored/utilities/indexparser.cpp: name, kind, class, and the")
    print("# index name ORE's parser is asked for, which carries the tenor only an")
    print("# Ibor index needs.")
    print("name\tkind\tclass\tprobe")
    for name, kind, klass in rows:
        probe = name if kind == "overnight" else f"{name}{PROBE_TENOR}"
        print(f"{name}\t{kind}\t{klass}\t{probe}")
    overnight = sum(1 for r in rows if r[1] == "overnight")
    print(f"# {len(rows)} entries: {overnight} overnight, {len(rows) - overnight} Ibor",
          file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
