#!/usr/bin/env python3
"""Generate the coverage note: what ORE does with each trade type, and what ORE
Studio holds of it.

Reads only files in the repository:

- external/ore/catalogue/trade_types.tsv, mined from ORE by
  external/ore/tools/datum_catalogue (what ORE registers, the class and form it
  reads each type as, the data element, and the library scripts that carry the
  same product).
- The trade type catalogue, for the form ORE Studio states and the instrument
  table that holds each type.
- The recorded generated trades, for whether the generator books a type.
- The task files of the coverage story, for the task that closes each gap: a
  task closes a type when its description names it.

Run::

    python3 build/scripts/generate_trade_type_coverage.py --write
    python3 build/scripts/generate_trade_type_coverage.py --check
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
MINED = REPO / "external/ore/catalogue/trade_types.tsv"
VERSION = REPO / "external/ore/catalogue/ore_version.txt"
CATALOGUE = REPO / "projects/ores.trading/modeling/ores.trading.trade_type_catalogue.org"
RECORDED = REPO / "projects/ores.synthetic/core/tests/data/generated_trades"
STORY = REPO / "doc/agile/versions/v0/sprint_30/hold_every_product_type_the_engine_supports"
DOCUMENT = REPO / "doc/knowledge/domain/ore_trade_type_coverage.org"
TAG_TASK = "80315472-7360-4C53-B6F7-57F7BCE78A1C"
FORM_TASK = "8F1B7D90-0D95-405B-845C-207AA1C92BE3"
BOTH_FORMS_TASK = "589884B5-1B2C-4044-AAD4-40680C9F3F0C"
BEGIN = "# BEGIN GENERATED"
END = "# END GENERATED"


def mined_rows() -> list[dict[str, str]]:
    lines = MINED.read_text().splitlines()
    header = lines[0].split("\t")
    return [dict(zip(header, line.split("\t"))) for line in lines[1:]]


class SourceError(Exception):
    pass


def catalogue() -> dict[str, dict[str, str]]:
    rows, inside, header = {}, False, None
    for line in CATALOGUE.read_text().splitlines():
        if line.startswith("* "):
            inside = line.strip() == "* Trade types"
            continue
        if not inside or not line.startswith("| "):
            continue
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if header is None:
            header = cells
            missing = {"code", "construction", "instrument"} - set(header)
            if missing:
                raise SourceError(f"{CATALOGUE.relative_to(REPO)}: the trade types table has no column"
                                  f" {', '.join(sorted(missing))}")
            continue
        row = dict(zip(header, cells))
        rows[row["code"]] = {"construction": row["construction"], "instrument": row["instrument"]}
    return rows


def field(path: Path, text: str, pattern: str) -> str:
    found = re.search(pattern, text, re.MULTILINE)
    if not found:
        raise SourceError(f"{path.relative_to(REPO)}: no line matches {pattern!r}")
    return found.group(1)


def story_tasks() -> list[tuple[str, str, str]]:
    tasks, ids = [], set()
    for path in sorted(STORY.glob("task_*.org")):
        text = path.read_text()
        uid = field(path, text, r":ID: (\S+)")
        ids.add(uid)
        if field(path, text, r"\| State\s+\| (\w+)") == "ABANDONED":
            continue
        tasks.append((uid, field(path, text, r"^#\+title: Task: (.*)"),
                      field(path, text, r"^#\+description: (.*)")))
    for uid in (TAG_TASK, FORM_TASK, BOTH_FORMS_TASK):
        if uid not in ids:
            raise SourceError(f"task {uid} is not in {STORY.relative_to(REPO)}; update the task constants")
    return tasks


# The catalogue constructions each form ORE's trade factory reports allows.
FORMS = {"native": {"native", "container"}, "scripted": {"scripted", "script"}}


def disagrees(row: dict[str, str], entry: dict[str, str]) -> bool:
    if row["registered"] != "yes":
        return False
    if row["form"] not in FORMS:
        raise SourceError(f"{MINED.relative_to(REPO)}: {row['trade_type']} has form {row['form']!r},"
                          f" which is not one of {', '.join(sorted(FORMS))}")
    return entry["construction"] not in FORMS[row["form"]]


def closing(row: dict[str, str], entry: dict[str, str], tasks) -> str:
    """The open tasks that close a type's gaps: one per kind of gap, and any
    task whose description names the type."""
    kinds = set()
    if row["registered"] == "no":
        kinds.add(TAG_TASK)
    if disagrees(row, entry):
        kinds.add(FORM_TASK)
    if row["library_scripts"] != "-":
        kinds.add(BOTH_FORMS_TASK)
    named = [f"[[id:{uid}][{title}]]" for uid, title, description in tasks
             if uid in kinds or re.search(rf"\b{re.escape(row['trade_type'])}\b", description)]
    return ", ".join(named) or "-"


def code(value: str) -> str:
    return "-" if value in ("", "-") else f"={value}="


def form_of(row: dict[str, str]) -> str:
    if row["registered"] == "no":
        return "not registered"
    return row["form"]


def table(rows, cat, tasks) -> list[str]:
    out = ["| Type | ORE form | Catalogue form | Data element | Table | Generated | Library scripts | Match | Closed by |",
           "|-"]
    for row in rows:
        if row["in_catalogue"] == "no":
            continue
        trade_type = row["trade_type"]
        entry = cat[trade_type]
        generated = "yes" if (RECORDED / f"{trade_type}.xml").exists() else "no"
        element = code(row["element"]) + ("" if row["element_in_schema"] == "yes" else " (not in the XSD)")
        scripts = ", ".join(code(s) for s in row["library_scripts"].split()) if row["library_scripts"] != "-" else "-"
        out.append("| " + " | ".join([
            code(trade_type), form_of(row), entry["construction"], element,
            code(entry["instrument"]), generated, scripts, row["script_match"], closing(row, entry, tasks),
        ]) + " |")
    return out


def counts(rows, cat) -> list[str]:
    declared = [r for r in rows if r["in_catalogue"] == "yes"]
    registered = [r for r in declared if r["registered"] == "yes"]
    scripted = [r for r in registered if r["form"] == "scripted"]
    routed = [r for r in declared if cat[r["trade_type"]]["instrument"]]
    generated = [r for r in declared if (RECORDED / f"{r['trade_type']}.xml").exists()]
    disagree = [r for r in registered if disagrees(r, cat[r["trade_type"]])]
    twins = [r for r in declared if r["library_scripts"] != "-"]
    only_ore = [r["trade_type"] for r in rows if r["in_catalogue"] == "no"]
    return [
        f"- The catalogue declares {len(declared)} types. ORE registers {len(registered)} of them,"
        f" {len(scripted)} read through a =ScriptedTrade= class and {len(registered) - len(scripted)} natively.",
        f"- {len(declared) - len(registered)} catalogue types have no ORE trade builder:"
        f" {', '.join(code(r['trade_type']) for r in declared if r['registered'] == 'no')}.",
        f"- ORE registers {len(only_ore)} type the catalogue leaves out: {', '.join(code(t) for t in only_ore)}.",
        f"- {len(routed)} types name an instrument table, and the generator records {len(generated)}.",
        f"- {len(disagree)} registered types state a form in the catalogue that ORE does not read them as.",
        f"- {len(twins)} types have a library script that carries the same product.",
    ]


def render() -> list[str]:
    rows = mined_rows()
    cat = catalogue()
    tasks = story_tasks()
    commit = VERSION.read_text().split()[2]
    return [BEGIN, "",
            f"Read at ORE commit ={commit}=. Regenerate with =build/scripts/generate_trade_type_coverage.py --write=.",
            "", "** Counts", "", *counts(rows, cat), "", "** Trade types", "", *table(rows, cat, tasks), "", END]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--write", action="store_true", help="rewrite the generated block")
    mode.add_argument("--check", action="store_true", help="fail when the generated block is stale")
    args = parser.parse_args()
    try:
        block = "\n".join(render())
    except SourceError as error:
        print(error, file=sys.stderr)
        return 1
    if not (args.write or args.check):
        print(block)
        return 0
    text = DOCUMENT.read_text()
    if text.count(BEGIN) != 1 or text.count(END) != 1:
        print(f"{DOCUMENT} needs one '{BEGIN}' and one '{END}' line", file=sys.stderr)
        return 1
    rows, cat = mined_rows(), catalogue()
    wrong = [r["trade_type"] for r in rows if r["in_catalogue"] == "yes" and disagrees(r, cat[r["trade_type"]])]
    if wrong:
        print(f"{CATALOGUE.relative_to(REPO)} states a construction ORE does not read these types in: "
              f"{', '.join(wrong)}", file=sys.stderr)
        return 1
    head, tail = text.split(BEGIN)[0], text.split(END)[1]
    updated = head + block + tail
    if args.check:
        if updated != text:
            print(f"{DOCUMENT.relative_to(REPO)} is stale; run {Path(__file__).relative_to(REPO)} --write",
                  file=sys.stderr)
            return 1
        return 0
    DOCUMENT.write_text(updated)
    return 0


if __name__ == "__main__":
    sys.exit(main())
