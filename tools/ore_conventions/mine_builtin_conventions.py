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
"""Mine the index conventions ORE defines in code into one table.

The names come from =extract_builtin_indices.py=; the physical fields come
from ORE itself, which is the only way to be sure the seed says what the engine
does: =index_catalogue= is asked what it makes of each index name, and
=code_catalogue= what name ORE reports for each calendar and day counter code.

Three vocabularies meet here, and each is read from the artefact that owns it:

- the index address, from =oresmd_index_map.tsv=, which the codecs generate;
- the day counter, from the codes =refdata= holds, each turned into the name ORE
  reports for it and matched against the name the index reports;
- the fixing calendar, the same way, over the names =refdata_calendar_names=
  holds. A name several codes produce takes the code that spells the name
  itself; the few that need a decision are in =builtin_index_decisions.tsv=.

Both ORE binaries are arguments, because the engine is not vendored:

    ./tools/ore_conventions/mine_builtin_conventions.py \\
        --index-catalogue <path to index_catalogue> \\
        --code-catalogue <path to code_catalogue> \\
        --conventions <path to conventions.xml>

The result is committed; nothing reads the engine at build time.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]

BUILTIN_INDICES = HERE / "builtin_indices.tsv"
DECISIONS = HERE / "builtin_index_decisions.tsv"
INDEX_MAP = HERE / "oresmd_index_map.tsv"
CALENDAR_NAMES = (REPO / "projects" / "ores.sql" / "populate" / "refdata" /
                  "refdata_calendar_names_populate.sql")
DAY_COUNTERS = (REPO / "projects" / "ores.sql" / "populate" / "refdata" /
                "refdata_deposits_create.sql")

OUT_CATALOGUE = REPO / "external" / "ore" / "catalogue" / "index_builtins.jsonl"
OUT_FIELDS = HERE / "builtin_index_fields.tsv"

# The day count codes refdata stores, which are FpML's. The list is written out
# rather than read from a table because it is a closed vocabulary, and an index
# that needs one not listed here fails the run rather than guessing.
DAY_COUNTER_CODES = (
    "ACT/360", "ACT/365.FIXED", "ACT/365.ACTUAL", "ACT/ACT.ISDA", "ACT/ACT.ISMA",
    "ACT/ACT.AFB", "ACT/ACT.ICMA", "30/360", "30E/360", "30E/360.ISDA", "BUS/252",
)


def run(binary: Path, argv: list[str], stdin: str) -> str:
    """The output of @p binary, or why it refused."""
    result = subprocess.run([str(binary), *argv], input=stdin, text=True,
                            capture_output=True)
    if result.returncode != 0:
        raise SystemExit(f"{binary} failed: {result.stderr.strip()}")
    return result.stdout


def read_tsv(path: Path, columns: int) -> list[list[str]]:
    rows = []
    for number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if not line.strip() or line.startswith("#") or line.startswith("name\t"):
            continue
        parts = line.split("\t")
        if len(parts) != columns:
            raise SystemExit(f"{path}:{number}: expected {columns} columns: {line!r}")
        rows.append(parts)
    return rows


def sql_codes(path: Path) -> list[str]:
    """The codes a populate script inserts, in file order."""
    return re.findall(r"'([A-Za-z][A-Za-z0-9 ._()\-]*)'", path.read_text(encoding="utf-8"))


def names_of(code_catalogue: Path, kind: str, codes: list[str]) -> dict[str, list[str]]:
    """Every code that parses as @p kind, by the name ORE reports for it."""
    wanted = sorted({c for c in codes if c})
    if not wanted:
        raise SystemExit(f"no {kind} codes to read")
    out = run(code_catalogue, [], "".join(f"{kind}\t{c}\n" for c in wanted))
    by_name: dict[str, list[str]] = {}
    for line in out.splitlines():
        parts = line.split("\t")
        if len(parts) != 3 or parts[2].startswith("!"):
            continue
        by_name.setdefault(parts[2], []).append(parts[1])
    return by_name


def resolve(candidates: list[str], name: str, decisions: dict[str, str], what: str) -> str:
    """The code that stands for @p name, or why there is none."""
    if name in decisions:
        return decisions[name]
    if name in candidates:
        return name
    if len(candidates) == 1:
        return candidates[0]
    # Several codes can build one object: the calendar's own name, the ISO
    # country code, the currency and the exchange all resolve to it. A
    # descriptive code names the thing, so it wins over the aliases.
    descriptive = [c for c in candidates if " " in c or not c.isupper()]
    if descriptive:
        return max(descriptive, key=len)
    raise SystemExit(f"no decision for the {what} {name!r}; candidates: {candidates}")


def main(argv: list[str]) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--index-catalogue", required=True, type=Path)
    ap.add_argument("--code-catalogue", required=True, type=Path)
    ap.add_argument("--conventions", required=True, type=Path)
    args = ap.parse_args(argv)

    builtins = read_tsv(BUILTIN_INDICES, 4)
    index_map = {n: u for n, u in read_tsv(INDEX_MAP, 2)}
    decisions = {n: code for n, code in read_tsv(DECISIONS, 2)}
    calendar_decisions = {n: c for n, c in decisions.items() if "." not in n or True}

    probes = {row[3]: row for row in builtins}
    catalogue = run(args.index_catalogue, [str(args.conventions)],
                    "".join(f"{p}\n" for p in probes))
    OUT_CATALOGUE.write_text(catalogue, encoding="utf-8")
    parsed = {}
    for line in catalogue.splitlines():
        entry = json.loads(line)
        if entry.get("accepted") != "true":
            raise SystemExit(f"ORE refused {entry['key']}: {entry.get('error')}")
        parsed[entry["key"]] = entry

    calendars = names_of(args.code_catalogue, "calendar", sql_codes(CALENDAR_NAMES))
    counters = names_of(args.code_catalogue, "daycounter", list(DAY_COUNTER_CODES))

    rows = []
    for name, kind, klass, probe in builtins:
        entry = parsed[probe]
        if name not in index_map:
            raise SystemExit(f"{name} has no address in the index map")
        calendar = resolve(calendars.get(entry["fixingCalendar"], []),
                           entry["fixingCalendar"], calendar_decisions, "calendar")
        counter = resolve(counters.get(entry["dayCounter"], []),
                          entry["dayCounter"], decisions, "day counter")
        rows.append([name, kind, entry["settlementDays"], calendar, counter,
                     entry["convention"], entry["endOfMonth"], index_map[name]])

    lines = [
        "# The index conventions ORE defines in code, mined from the engine.",
        "# Regenerate with mine_builtin_conventions.py; see the task.",
        "name\tkind\tsettlement_days\tfixing_calendar\tday_counter\t"
        "convention\tend_of_month\tore_name\toresmd_uri",
    ]
    for name, kind, days, calendar, counter, convention, eom, uri in rows:
        lines.append(f"{name}\t{kind}\t{days}\t{calendar}\t{counter}\t"
                     f"{convention}\t{eom}\t{name}\t{uri}")
    OUT_FIELDS.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"wrote {len(rows)} rows to {OUT_FIELDS.relative_to(REPO)}", file=sys.stderr)
    print(f"wrote {OUT_CATALOGUE.relative_to(REPO)}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
