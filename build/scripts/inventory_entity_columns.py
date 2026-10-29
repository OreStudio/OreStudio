#!/usr/bin/env python3
"""Count the members and columns of every persisted type the models declare.

Why this exists
---------------

A persisted type is meant to hold one component's fields. Many of ours hold
thirty to sixty, and the compiler is the least of it: a type with sixty columns
says six things at once, and the screen, the job and the report that read it
each read a different subset. The C1202 that MSVC reported on 2026-10-29 -- a
std::tuple_cat over one column binding per column in the generated query
builder -- is a symptom of that shape, not a cause.

The story "Split the types that carry too many columns" needs a work list, and a
work list needs numbers that a reader can reproduce. This is the lever: it reads
the models, which are the authority, and prints one row per type.

How it counts
-------------

The model is the authority, not the generated header. For each
ores.codegen.entity model it reads the * Columns block and counts:

- columns   -- every column the model declares, in the table;
- nullable  -- the columns that declare :nullable: true;
- grouped   -- the columns that declare :group: <name>, which fold into a member
               of their own type rather than staying a flat member;
- members   -- what the domain struct carries: the ungrouped columns plus one
               member per distinct group;
- parts     -- the rows of a * Parts table, for a family's kind row.

members is the number that decides the type's shape; columns is the number that
decides what the database and the query builder pay. A type can be wide in one
and narrow in the other, so both are printed.

The generated query builder also binds the scaffold columns -- key, tenant,
version and the audit columns -- so its binding count is a few higher than
columns. Cross-check a row against the generated entity when the exact binding
count matters:

    grep -c 'query_builder::bind(' \\
        projects/ores.refdata/core/include/ores.refdata.core/repository/<entity>_entity.hpp

Usage
-----

    python3 build/scripts/inventory_entity_columns.py            # TSV, widest first
    python3 build/scripts/inventory_entity_columns.py --summary  # ranked table
    python3 build/scripts/inventory_entity_columns.py --threshold 20
"""

from __future__ import annotations

import argparse
import pathlib
import re

HEADING_RE = re.compile(r"^(\*{1,2}) (.*?)\s*$")
PROPERTY_RE = re.compile(r"^:([a-z_]+):\s*(.*?)\s*$")
HEADER_RE = re.compile(r"^#\+([a-z_]+):\s*(.*?)\s*$")
PART_ROW_RE = re.compile(r"^\|\s*[a-z_]+\s*\|\s*([a-z_]+)\s*\|")


def model_headers(path: pathlib.Path) -> dict[str, str]:
    headers: dict[str, str] = {}
    for line in path.read_text(errors="ignore").splitlines():
        if not line.startswith("#+"):
            continue
        header = HEADER_RE.match(line)
        if header:
            headers[header.group(1)] = header.group(2)
    return headers


def model_body(path: pathlib.Path) -> tuple[dict[str, str], dict[str, str], int]:
    """Return the Flags drawer, the column drawers by name, and the part count."""
    flags: dict[str, str] = {}
    columns: dict[str, dict[str, str]] = {}
    parts = 0
    section = ""
    in_properties = False
    in_flags = False
    current: str | None = None

    for line in path.read_text(errors="ignore").splitlines():
        heading = HEADING_RE.match(line)
        if heading:
            stars, name = heading.group(1), heading.group(2)
            in_properties = False
            if stars == "*":
                section = name
                in_flags = name == "Flags"
                current = None
                continue
            if section == "Columns":
                current = name
                columns.setdefault(name, {})
            continue

        if section in ("Columns", "Parts") and line.startswith("|"):
            part = PART_ROW_RE.match(line)
            if part and section == "Parts":
                parts += 1

        if line.strip() == ":PROPERTIES:":
            in_properties = True
            continue
        if line.strip() == ":END:":
            in_properties = False
            continue
        if not in_properties:
            continue
        prop = PROPERTY_RE.match(line)
        if not prop:
            continue
        key, value = prop.group(1), prop.group(2)
        if in_flags:
            flags[key] = value
        elif current is not None:
            columns[current][key] = value

    return flags, columns, parts


def rows() -> list[dict[str, str]]:
    found: list[dict[str, str]] = []
    for path in sorted(pathlib.Path("projects").glob("*/modeling/*.org")):
        headers = model_headers(path)
        if headers.get("type") != "ores.codegen.entity":
            continue
        flags, columns, parts = model_body(path)
        groups = {c["group"] for c in columns.values() if c.get("group")}
        grouped = sum(1 for c in columns.values() if c.get("group"))
        nullable = sum(1 for c in columns.values() if c.get("nullable") == "true")
        found.append(
            {
                "model": str(path),
                "component": headers.get("component", ""),
                "entity": headers.get("entity_singular", ""),
                "columns": str(len(columns)),
                "nullable": str(nullable),
                "grouped": str(grouped),
                "groups": ",".join(sorted(groups)),
                "members": str(len(columns) - grouped + len(groups)),
                "parts": str(parts),
                "profile": flags.get("profile", ""),
            }
        )
    found.sort(key=lambda r: (-int(r["members"]), -int(r["columns"]), r["model"]))
    return found


def print_tsv(found: list[dict[str, str]]) -> None:
    fields = ["model", "component", "entity", "members", "columns", "nullable",
              "grouped", "groups", "parts", "profile"]
    print("\t".join(fields))
    for row in found:
        print("\t".join(row[field] for field in fields))


def print_summary(found: list[dict[str, str]], threshold: int) -> None:
    wide = [r for r in found if int(r["members"]) >= threshold]
    print(f"{len(found)} persisted types; {len(wide)} carry {threshold} members or more.\n")
    print(f"{'members':>7} {'columns':>7} {'nullable':>8} {'parts':>5}  entity")
    for row in wide:
        print(f"{row['members']:>7} {row['columns']:>7} {row['nullable']:>8} "
              f"{row['parts']:>5}  {row['entity']} ({row['component']})")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--summary", action="store_true",
                        help="print the ranked table instead of the TSV")
    parser.add_argument("--threshold", type=int, default=20,
                        help="members at which a type is listed (default 20)")
    args = parser.parse_args()

    found = rows()
    if args.summary:
        print_summary(found, args.threshold)
    else:
        print_tsv(found)


if __name__ == "__main__":
    main()
