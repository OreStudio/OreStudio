#!/usr/bin/env python3
"""Check the market series identity projection against the codec schema.

The projection table has one column per identity field the codec schema
declares. The schema is the source of truth and the table is written by hand,
so the two can drift with nothing failing. This check closes that gap:

- The create table statement must carry exactly the identity fields the schema
  marks as identity, plus the columns that say which kind of identity the row
  is.
- The projector's switch must place every field the schema declares: an
  identity field writes its own column, a coordinate field writes nothing.

Run it bare. A finding is a mismatch, and the message names the column and the
direction each way.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

SCHEMA = REPO_ROOT / "projects/ores.marketdata/api/include/ores.marketdata.api/datum/schema.hpp"
PROJECTOR = REPO_ROOT / "projects/ores.marketdata/core/src/repository/market_series_identity_projector.cpp"
CREATE_SQL = REPO_ROOT / "projects/ores.sql/create/marketdata/marketdata_market_series_identity_create.sql"

# The entity's schema identifier, which the field's own name must not be:
# a field called table would collide with the statement's own words otherwise.
CREATE_TABLE = "ores_marketdata_market_series_identity_tbl"

# The columns that say what the identity is, beside the identity's own fields.
CONTEXT_COLUMNS = (
    "series_id",
    "tenant_id",
    "party_id",
    "identity_kind",
    "asset_class",
    "instrument_type",
    "quote_type",
)

# The schema lists a field as identity by writing id(f::x) or id_or_none(f::x)
# in its per-type row, and as a coordinate by writing at(f::x) or
# at_or_none(f::x). The alias the rows use.
IDENTITY_REF = re.compile(r"\bid(?:_or_none)?\(f::(\w+)\)")
COORDINATE_REF = re.compile(r"\bat(?:_or_none)?\(f::(\w+)\)")
# The whole enum, so a field the rows never mention is still seen.
ENUM_MEMBER = re.compile(r"^\s*(\w+),\s*$", re.M)

ALL_FIELDS_ANCHOR = "enum class field"


def fail(problems: list[str]) -> int:
    for p in problems:
        print(f"FAIL: {p}")
    return 1


def schema_fields() -> tuple[set[str], set[str], set[str]]:
    """(identity, coordinate, every declared field), from the codec schema."""
    text = SCHEMA.read_text()
    if "using f = field;" not in text:
        raise SystemExit(
            f"FAIL: {SCHEMA.relative_to(REPO_ROOT)} no longer declares 'using f = field;', "
            "which this check reads the per-type rows through."
        )
    rows = text[text.index("using f = field;") :]

    enum_start = text.index(ALL_FIELDS_ANCHOR)
    enum_end = text.index("};", enum_start)
    every = set(ENUM_MEMBER.findall(text[enum_start:enum_end]))

    identity = set(IDENTITY_REF.findall(rows))
    coordinate = set(COORDINATE_REF.findall(rows)) - identity
    if not every or not identity:
        raise SystemExit(
            f"FAIL: parsed no fields from {SCHEMA.relative_to(REPO_ROOT)}; the check needs fixing."
        )
    return identity, coordinate, every


def table_columns() -> set[str]:
    """The columns of the projection's create table statement."""
    text = CREATE_SQL.read_text()
    if f'"{CREATE_TABLE}"' not in text:
        raise SystemExit(f"FAIL: {CREATE_SQL.relative_to(REPO_ROOT)} does not create {CREATE_TABLE}.")
    body = text[text.index(f'"{CREATE_TABLE}" (') :]
    body = body[body.index("(") + 1 :]
    depth = 1
    columns: set[str] = set()
    for line in body.splitlines():
        depth += line.count("(") - line.count(")")
        if depth <= 0:
            break
        m = re.match(r'\s*"([a-z_0-9]+)"\s', line)
        if m and not line.strip().startswith(("primary key", "check", "exclude", "unique")):
            columns.add(m.group(1))
    if not columns:
        raise SystemExit(
            f"FAIL: parsed no columns from {CREATE_SQL.relative_to(REPO_ROOT)}; the check needs fixing."
        )
    return columns


def projector_switch() -> dict[str, str]:
    """field -> the column it writes, for every case the switch names."""
    text = PROJECTOR.read_text()
    if "switch (f) {" not in text:
        raise SystemExit(
            f"FAIL: {PROJECTOR.relative_to(REPO_ROOT)} no longer switches on the field; "
            "this check reads the placement through that switch."
        )
    body = text[text.index("switch (f) {") :]
    body = body[: body.index("\n    }\n")]
    return {
        m.group(1): (m.group(2) or "")
        for m in re.finditer(
            r"case field::(\w+):\n\s*(?:row\.(\w+) = text;)?\s*\n?\s*break;", body
        )
    }


def main() -> int:
    identity, coordinate, every = schema_fields()
    problems: list[str] = []

    # 1. The table's columns are exactly the context columns and the identity
    #    fields, and nothing else.
    columns = table_columns()
    expected = set(CONTEXT_COLUMNS) | identity
    for missing in sorted(expected - columns):
        problems.append(f"{CREATE_TABLE} has no column '{missing}'.")
    for extra in sorted(columns - expected):
        problems.append(f"{CREATE_TABLE} has column '{extra}', which is not an identity field.")

    # 2. The projector places every field, and places each one correctly.
    placed = projector_switch()
    for f in sorted(every):
        if f not in placed:
            problems.append(f"the projector's switch does not name field '{f}'.")
            continue
        written = placed[f]
        if f in identity and written != f:
            problems.append(
                f"the projector places field '{f}' in column '{written or 'nothing'}'; "
                f"it is an identity field and belongs in '{f}'."
            )
        if f in coordinate and written:
            problems.append(
                f"the projector places coordinate field '{f}' in column '{written}'; "
                "a series holds no coordinate."
            )

    if problems:
        return fail(problems)
    print(
        f"OK: {len(identity)} identity fields, {len(columns)} columns and "
        f"{len(placed)} switch cases agree with {SCHEMA.relative_to(REPO_ROOT)}."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
