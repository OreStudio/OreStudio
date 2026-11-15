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
"""Compare the refdata.conventions dataset with the conventions ORE holds.

ORE holds conventions in two places: its examples, which
tools/ore_conventions/extract.py extracts to
tmp/ore_conventions/conventions-canonical.tsv, and its engine, whose index
conventions tools/ore_conventions/mine_builtin_conventions.py mines to
tools/ore_conventions/builtin_index_fields.tsv. This script reports each of
them the dataset lacks, and each whose terms differ from the dataset's row.
propose_decisions.tsv, beside it, states why the dataset keeps a row of its own;
a difference it names is not reported.

It also checks the table itself: each (kind, id) is one row, each row names its
source, and each Ibor or overnight index a convention names is defined by an
index convention of the table, matched by its address without the tenor, as the
convention lookup matches it.

    ./projects/ores.codegen/venv/bin/python tools/ore_conventions/extract.py
    python3 projects/ores.seeder/datasets/conventions/propose.py [--check]

With --check it exits 1 when it reports anything, so a gate can run it.
"""

from __future__ import annotations

import argparse
import csv
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
DATASET = HERE / "conventions.tsv"
DECISIONS = HERE / "propose_decisions.tsv"
CORPUS = ROOT / "tmp" / "ore_conventions" / "conventions-canonical.tsv"
ENGINE = ROOT / "tools" / "ore_conventions" / "builtin_index_fields.tsv"

# The spellings ORE's examples use for a business day convention, which the
# engine states in full.
BDC = {"Following": "F", "ModifiedFollowing": "MF",
       "HalfMonthModifiedFollowing": "HMMF"}

INDEX_MAP = HERE / "oresmd_index_map.tsv"
# The fields whose value names an index, as the generator reads them.
INDEX_FIELDS = ("Index", "BMAIndex", "FlatIndex", "SpreadIndex", "IndexName",
                "PayIndex", "ReceiveIndex", "LongIndex", "ShortIndex")

ENGINE_SOURCE = ("ORE engine: OREData/ored/utilities/indexparser.cpp, mined by "
                 "tools/ore_conventions/mine_builtin_conventions.py")


def read_tsv(path: Path) -> list[dict]:
    with open(path, newline="", encoding="utf-8") as fh:
        lines = [line for line in fh if not line.startswith("#")]
    return [r for r in csv.DictReader(lines, delimiter="\t") if r.get("kind")]


def engine_signature(row: dict) -> tuple[str, str]:
    """The kind and the ORE-notation signature of a mined index convention."""
    kind = "IborIndex" if row["kind"] == "ibor" else "OvernightIndex"
    parts = []
    if kind == "IborIndex":
        parts += [f"BusinessDayConvention={BDC.get(row['convention'], row['convention'])}",
                  f"DayCounter={row['day_counter']}", f"EndOfMonth={row['end_of_month']}"]
    else:
        parts += [f"DayCounter={row['day_counter']}"]
    parts += [f"FixingCalendar={row['fixing_calendar']}",
              f"SettlementDays={row['settlement_days']}"]
    return kind, " ; ".join(sorted(parts))


def ore_conventions() -> list[tuple[str, str, str, str]]:
    """Each convention ORE holds, as (origin, kind, id, signature)."""
    out = []
    if not CORPUS.exists():
        raise SystemExit(f"{CORPUS.relative_to(ROOT)} is missing: run "
                         "tools/ore_conventions/extract.py first")
    for r in read_tsv(CORPUS):
        out.append(("ORE examples", r["kind"], r["id"], r["signature"]))
    for r in read_tsv(ENGINE):
        kind, signature = engine_signature({**r, "kind": r["kind"],
                                            "convention": r["convention"]})
        out.append(("ORE engine", kind, r["name"], signature))
    return out


def table_problems(rows: list[dict]) -> list[str]:
    problems = []
    seen = set()
    for r in rows:
        key = (r["kind"], r["id"])
        if key in seen:
            problems.append(f"{r['kind']} {r['id']}: more than one row")
        seen.add(key)
        if not r.get("source", "").strip():
            problems.append(f"{r['kind']} {r['id']}: names no source")
    return problems


def family_of(uri: str) -> str:
    """The address of an index's family: the address without its tenor."""
    return re.sub(r"&tenor=[^&]*", "", uri)


def undefined_indices(rows: list[dict]) -> list[str]:
    """Each Ibor or overnight index a convention names that no index convention defines."""
    index_map = {}
    for line in INDEX_MAP.read_text(encoding="utf-8").splitlines():
        parts = line.split("\t")
        if len(parts) == 2:
            index_map[parts[0]] = parts[1]
    defined = set()
    for r in rows:
        if r["kind"] in ("IborIndex", "OvernightIndex"):
            uri = r.get("oresmd_uri") or index_map.get(r["id"], "")
            if uri:
                defined.add(family_of(uri))
    out = []
    for r in rows:
        fields = dict(p.split("=", 1) for p in r["signature"].split(" ; ") if "=" in p)
        for field in INDEX_FIELDS:
            name = fields.get(field, "").strip()
            uri = index_map.get(name, "")
            if "index=ibor" in uri and family_of(uri) not in defined:
                out.append(f"{r['kind']} {r['id']}: names {name}, which no Ibor or "
                           "overnight index convention of the dataset defines")
    return out


def differences(rows: list[dict], ore: list[tuple[str, str, str, str]],
                decided: set[tuple[str, str, str]]) -> list[str]:
    held = {(r["kind"], r["id"]): r["signature"] for r in rows}
    out = []
    for origin, kind, cid, signature in ore:
        if (origin, kind, cid) in decided:
            continue
        if (kind, cid) not in held:
            out.append(f"{origin}: {kind} {cid} is not in the dataset")
        elif held[(kind, cid)] != signature:
            out.append(f"{origin}: {kind} {cid} differs: ORE states '{signature}', "
                       f"the dataset '{held[(kind, cid)]}'")
    return out


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--check", action="store_true",
                        help="exit 1 when anything is reported")
    args = parser.parse_args(argv)

    rows = read_tsv(DATASET)
    decided = {(r["origin"], r["kind"], r["id"]) for r in read_tsv(DECISIONS)}
    report = (table_problems(rows) + undefined_indices(rows) +
              differences(rows, ore_conventions(), decided))
    for line in report:
        print(line)
    print(f"{len(rows)} row(s); {len(report)} finding(s)")
    return 1 if args.check and report else 0


if __name__ == "__main__":
    sys.exit(main())
