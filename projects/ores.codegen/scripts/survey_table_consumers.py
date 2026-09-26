#!/usr/bin/env python3
"""
Print who reads and who writes each artefact table of one SQL product.

Step 2 of the ores.dq clean-up decides, for every artefact table, whether
it is kept, modelled or deleted. A generated marker does not answer that
question: 13 generated tables have no model, and one of them,
=asset_classes=, is live. The only safe basis for the decision is a
consumer census.

The scan reads every text file once and records, for each artefact table
it mentions, the relation the file has to that table:

  defines     a CREATE TABLE for the table itself.
  writes      an INSERT into the table, or an UPDATE of it.
  reads       a FROM or JOIN reference, including a publish function.
  drops       a DROP TABLE.
  registers   a dataset or artefact-type registration that names it.
  cpp         a C++ source, which usually means sqlgen built the SQL.
  doc         a document.
  mentions    a reference none of the above explains. Not proof of use.

The relation comes from the statement next to the name, not from the
directory the file sits in. A file under =create/= is not automatically a
definition: =dq_asset_classes_population_functions_create.sql= publishes
from the table and =refdata_publish_from_dq_create.sql= reads it.

A table name is also spelled two ways. The create tree writes it in full
(=ores_dq_currencies_artefact_tbl=); the artefact-type registry writes it
short (=dq_currencies_artefact_tbl=). Both are searched, because the
registry is the authoritative record of what the publish pipeline serves.

A table with no =writes= and no =reads= is a deletion candidate. A table
with either is kept, and modelled.

Read-only. Nothing is written, no file is created, and no model,
catalogue entry or generated file is touched.

Usage:
  survey_table_consumers.py
  survey_table_consumers.py --product dq
  survey_table_consumers.py --product dq --table ores_dq_asset_classes_artefact_tbl
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]

_TBL_SUFFIX = "_tbl"

_SCANNED_SUFFIXES = (
    ".sql", ".org", ".hpp", ".cpp", ".md", ".py", ".sh", ".json",
    ".ts", ".tsx", ".cmake", ".txt", ".mustache",
)

_PRUNED_DIRS = frozenset(
    {".git", "build", "venv", ".venv", "node_modules", "tmp", "__pycache__", ".cache"}
)

# Roles that prove something other than the definition touches the table.
_LIVE_ROLES = ("writes", "reads", "registers", "cpp")


def scanned_files(create_dir: Path) -> list:
    """Every text file worth scanning, outside generated and vendored trees."""
    found: list = []
    for path in REPO_ROOT.rglob("*"):
        if not path.is_file():
            continue
        if any(part in _PRUNED_DIRS for part in path.parts):
            continue
        if path.suffix not in _SCANNED_SUFFIXES:
            continue
        found.append(path)
    return sorted(found)


def aliases_for(table: str) -> list:
    """Every spelling of one table name that appears in the tree.

    The create tree spells the name in full: =ores_dq_currencies_artefact_tbl=.
    The artefact-type registry spells it short: =dq_currencies_artefact_tbl=.
    Missing the short form would miss the registry, which is the
    authoritative record of what the publish pipeline serves.
    """
    names = [table]
    if table.startswith("ores_"):
        names.append(table[len("ores_"):])
    return names


def relation(text: str, table: str, path: Path) -> str:
    """How one file relates to one table, from the SQL around the name.

    A JOIN is a read. A publish function reads the artefact table and
    writes the base table, so a name that appears only after FROM or JOIN
    is still a reason to keep the table.
    """
    quoted = rf'"?{re.escape(table)}"?'
    if re.search(rf"create\s+table\s+(?:if\s+not\s+exists\s+)?{quoted}", text, re.IGNORECASE):
        return "defines"
    if re.search(rf"drop\s+table\s+(?:if\s+exists\s+)?{quoted}", text, re.IGNORECASE):
        return "drops"
    if path.suffix in (".hpp", ".cpp"):
        return "cpp"
    if path.suffix in (".org", ".md"):
        return "doc"
    if "populate" in path.parts and "artefact_types" in path.name:
        return "registers"
    if re.search(rf"(?:insert\s+into|update)\s+{quoted}", text, re.IGNORECASE):
        return "writes"
    if re.search(rf"(?:from|join)\s+{quoted}", text, re.IGNORECASE):
        return "reads"
    return "mentions"


def build_index(files: list, wanted: set) -> dict:
    """table name -> {role: [relative paths]} for the tables of interest."""
    alias_to_table: dict = {}
    for table in wanted:
        for alias in aliases_for(table):
            alias_to_table[alias] = table
    if not alias_to_table:
        return {}

    finder = re.compile(
        "|".join(re.escape(a) for a in sorted(alias_to_table, key=len, reverse=True))
    )

    index: dict = {}
    for path in files:
        try:
            text = path.read_text(encoding="utf-8")
        except (UnicodeDecodeError, OSError):
            continue
        matched = set(finder.findall(text))
        if not matched:
            continue
        rel = str(path.relative_to(REPO_ROOT))
        # The relation is read against the spelling the file actually uses,
        # then attributed to the canonical full name.
        for alias in matched:
            name = alias_to_table[alias]
            index.setdefault(name, {}).setdefault(relation(text, alias, path), []).append(rel)
    return index


def parse_artefact_tables(create_dir: Path) -> list:
    """Every artefact table the product declares, with the file that declares it."""
    found: list = []
    pattern = re.compile(
        r"create\s+table\s+(?:if\s+not\s+exists\s+)?\"(?P<table>[^\"]+)\"",
        re.IGNORECASE,
    )
    seen: set = set()
    for path in sorted(create_dir.glob("*.sql")):
        text = path.read_text(encoding="utf-8")
        for match in pattern.finditer(text):
            table = match.group("table")
            if not table.endswith("_artefact_tbl") or table in seen:
                continue
            seen.add(table)
            found.append((table, str(path.relative_to(REPO_ROOT))))
    return found


def own_file_mentions(roles: dict, table: str, own_file: str) -> dict:
    """Drop the defining file from its own ``defines`` and ``drops`` roles.

    A table always defines itself. Counting that as a consumer would make
    every table look live.
    """
    trimmed: dict = {}
    for role, rels in roles.items():
        kept = [r for r in rels if not (r == own_file and role in ("defines", "drops"))]
        if kept:
            trimmed[role] = sorted(kept)
    return trimmed


def verdict(roles: dict) -> str:
    live = sorted(set(roles) & set(_LIVE_ROLES))
    if not live:
        return "DELETE CANDIDATE"
    return "keep: " + ", ".join(live)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--product", default="dq", help="SQL product (default: dq)")
    ap.add_argument("--table", default=None, help="Report one table only")
    args = ap.parse_args()

    create_dir = REPO_ROOT / "projects" / "ores.sql" / "create" / args.product
    if not create_dir.is_dir():
        print(f"no such product: {create_dir}", file=sys.stderr)
        return 1

    artefacts = parse_artefact_tables(create_dir)
    if args.table:
        artefacts = [a for a in artefacts if a[0] == args.table]

    files = scanned_files(create_dir)
    index = build_index(files, {t for t, _ in artefacts})

    print(f"# Artefact table consumers: projects/ores.sql/create/{args.product}")
    print()
    print(f"Scanned {len(files)} file(s); {len(artefacts)} artefact table(s).")
    print()
    print("| artefact table | verdict | writes | reads | registers | drops | cpp | doc | mentions |")
    print("|----------------|---------|--------|-------|-----------|-------|-----|-----|----------|")
    for table, own_file in artefacts:
        roles = own_file_mentions(index.get(table, {}), table, own_file)
        counts = {k: len(roles.get(k, [])) for k in
                  ("writes", "reads", "registers", "drops", "cpp", "doc", "mentions")}
        print(
            f"| {table} | {verdict(roles)} | {counts['writes']} | {counts['reads']} | "
            f"{counts['registers']} | {counts['drops']} | {counts['cpp']} | {counts['doc']} | "
            f"{counts['mentions']} |"
        )
    print()

    for table, own_file in artefacts:
        roles = own_file_mentions(index.get(table, {}), table, own_file)
        if not roles:
            continue
        print(f"## {table}")
        print()
        for role in ("writes", "reads", "registers", "drops", "cpp", "doc", "mentions", "defines"):
            for rel in roles.get(role, []):
                print(f"- {role}: {rel}")
        print()

    return 0


if __name__ == "__main__":
    sys.exit(main())
