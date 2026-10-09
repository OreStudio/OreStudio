#!/usr/bin/env python3
"""Check the market series identity projection against the codec schema.

The projection table has one column per identity field the codec schema
declares. The schema is the source of truth and the table is written by hand,
so the two can drift with nothing failing. This check closes that gap:

- The create table statement must carry exactly the identity fields the schema
  marks as identity, the columns the index grammar's fields land in, plus the
  columns that say which kind of identity the row is.
- The projector's switch must place every field the schema declares: an
  identity field writes its own column, a coordinate field writes nothing.
- The projector must place every name the index grammar declares, and each name
  must land in a real column. Every index name is part of a fixing's identity,
  because ORE writes it into the index name, so a name the projector does not
  place cannot be dropped without a failure.

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
VIEWS_SQL = REPO_ROOT / "projects/ores.sql/create/marketdata/marketdata_series_identity_views_create.sql"
INDEX_GRAMMAR = REPO_ROOT / "projects/ores.marketdata/api/include/ores.marketdata.api/datum/market_index.hpp"

# The columns every asset-class view carries beside its class's own fields.
VIEW_COLUMNS = (
    "series_id",
    "tenant_id",
    "party_id",
    "instrument_type",
    "quote_type",
    "asset_class",
    "identity_kind",
)

# A per-type row names its field list, its asset class and its subject:
# {detail::t::cds, detail::cds, asset_class::credit, field::underlying_name}
SCHEMA_ROW = re.compile(
    r"\{detail::t::(\w+),\s*detail::(\w+),\s*asset_class::(\w+),\s*field::(\w+)\}"
)
SCHEMA_ARRAY = re.compile(r"inline constexpr std::array (\w+)\{(.*?)\};", re.S)

VIEW = re.compile(
    r"create or replace view (\w+)\s+"
    r"with \(security_invoker = true\) as\s+select\s+(.*?)\s+"
    r"from ores_marketdata_market_series_identity_tbl\s+"
    r"where identity_kind = 'series'\s+"
    r"and instrument_type in \(\s*(.*?)\s*\);",
    re.S,
)
VIEW_NAME = re.compile(r"^ores_marketdata_series_identity_(\w+)_vw$")

# An index row names its family, its asset class, its subject and its field
# array: {index_family::ibor, asset_class::ir, "ccy", detail::ibor_fields}
INDEX_ROW = re.compile(
    r"\{index_family::(\w+),\s*asset_class::(\w+),\s*\"(\w+)\",\s*detail::(\w+)\}"
)
# A family's field array lists its fields as {{"name", optional}, ...}.
INDEX_FIELD_ARRAY = re.compile(
    r"inline constexpr std::array<index_field_spec, \d+> (\w+)\{(.*?)\};", re.S
)
INDEX_FIELD_NAME = re.compile(r"\{\"(\w+)\",\s*(?:true|false)\}")
# The projector places an index name with a comparison on the name and an
# assignment of its text to the column that holds it.
INDEX_PLACEMENT = re.compile(r'name == "(\w+)"\)\s*\n\s*row\.(\w+) = text;')

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


def schema_classes() -> dict[str, tuple[set[str], set[str]]]:
    """class -> (identity fields, instrument types), from the codec schema."""
    text = SCHEMA.read_text()
    rows = text[text.index("using f = field;") :]

    arrays: dict[str, set[str]] = {}
    for m in SCHEMA_ARRAY.finditer(rows):
        arrays[m.group(1)] = set(IDENTITY_REF.findall(m.group(2)))

    classes: dict[str, tuple[set[str], set[str]]] = {}
    for _type, array, asset, _subject in SCHEMA_ROW.findall(rows):
        fields, types = classes.setdefault(asset, (set(), set()))
        fields.update(arrays.get(array, set()))
        types.add(_type)
    if not classes:
        raise SystemExit(
            f"FAIL: parsed no asset classes from {SCHEMA.relative_to(REPO_ROOT)}; "
            "the check needs fixing."
        )
    return classes


def views() -> dict[str, tuple[list[str], set[str]]]:
    """class -> (column names, instrument types), for every view the file holds."""
    text = VIEWS_SQL.read_text()
    found: dict[str, tuple[list[str], set[str]]] = {}
    for name, columns_text, types_text in VIEW.findall(text):
        m = VIEW_NAME.match(name)
        if not m:
            raise SystemExit(
                f"FAIL: {VIEWS_SQL.relative_to(REPO_ROOT)} defines '{name}', which does not "
                "follow ores_marketdata_series_identity_<class>_vw."
            )
        # A view exposes the field's own name. A field the table renames, because
        # its name is a SQL word the store refuses, is selected under that name
        # and aliased back, so the exposed column is the alias when there is one.
        columns = []
        for item in columns_text.split(","):
            item = item.strip()
            if not item:
                continue
            lowered = item.lower()
            if " as " in lowered:
                item = item[len(lowered.split(" as ")[0]) + 4 :]
            columns.append(item.strip().strip('"'))
        types = set(re.findall(r"'(\w+)'", types_text))
        found[m.group(1)] = (columns, types)
    if not found:
        raise SystemExit(
            f"FAIL: parsed no views from {VIEWS_SQL.relative_to(REPO_ROOT)}; the check needs fixing."
        )
    return found


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


def index_grammar_names() -> set[str]:
    """Every name the index grammar declares: each family's subject and fields."""
    text = INDEX_GRAMMAR.read_text()
    arrays = {
        m.group(1): set(INDEX_FIELD_NAME.findall(m.group(2)))
        for m in INDEX_FIELD_ARRAY.finditer(text)
    }
    names: set[str] = set()
    for _family, _asset, subject, array in INDEX_ROW.findall(text):
        names.add(subject)
        names.update(arrays.get(array, set()))
    if not names:
        raise SystemExit(
            f"FAIL: parsed no index names from {INDEX_GRAMMAR.relative_to(REPO_ROOT)}; "
            "the check needs fixing."
        )
    return names


def projector_index_mapping() -> dict[str, str]:
    """name -> the projection column the projector writes it into."""
    text = PROJECTOR.read_text()
    if "void assign_index(" not in text:
        raise SystemExit(
            f"FAIL: {PROJECTOR.relative_to(REPO_ROOT)} no longer defines assign_index; "
            "this check reads the index mapping through it."
        )
    body = text[text.index("void assign_index(") :]
    body = body[: body.index("\n}\n")]
    placed = dict(INDEX_PLACEMENT.findall(body))
    if not placed:
        raise SystemExit(
            f"FAIL: {PROJECTOR.relative_to(REPO_ROOT)} no longer names the index mapping in "
            "the shape this check reads."
        )
    return placed


def main() -> int:
    identity, coordinate, every = schema_fields()
    problems: list[str] = []

    index_names = index_grammar_names()
    index_placed = projector_index_mapping()
    index_columns = set(index_placed.values())

    # 1. The table's columns are exactly the context columns, the schema's
    #    identity fields and the columns the index grammar's fields land in,
    #    and nothing else.
    columns = table_columns()
    expected = set(CONTEXT_COLUMNS) | identity | index_columns
    for missing in sorted(expected - columns):
        problems.append(f"{CREATE_TABLE} has no column '{missing}'.")
    for extra in sorted(columns - expected):
        problems.append(
            f"{CREATE_TABLE} has column '{extra}', which is neither a context column, "
            "nor an identity field, nor an index field."
        )

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

    # 3. Each asset class has a view whose columns and instrument types are
    #    exactly the class's.
    classes = schema_classes()
    found = views()
    for asset in sorted(set(classes) - set(found)):
        problems.append(f"no view exposes the {asset} identity.")
    for asset in sorted(set(found) - set(classes)):
        problems.append(f"a view exposes the {asset} identity, which the schema gives no series.")
    for asset in sorted(set(classes) & set(found)):
        class_fields, class_types = classes[asset]
        view_columns, view_types = found[asset]
        if len(view_columns) != len(set(view_columns)):
            problems.append(f"the {asset} view selects a column more than once.")
        context = set(VIEW_COLUMNS)
        for missing in sorted(context - set(view_columns)):
            problems.append(f"the {asset} view does not expose '{missing}'.")
        for extra in sorted(set(view_columns) - context - class_fields):
            problems.append(
                f"the {asset} view exposes column '{extra}', which is not one of its "
                "identity fields."
            )
        for missing in sorted(class_fields - set(view_columns)):
            problems.append(f"the {asset} view does not expose identity field '{missing}'.")
        for extra in sorted(view_types - class_types):
            problems.append(
                f"the {asset} view names instrument type '{extra}', which does not "
                f"belong to {asset}."
            )
        for missing in sorted(class_types - view_types):
            problems.append(
                f"the {asset} view does not name instrument type '{missing}', so its "
                "series are missing from it."
            )

    # 4. The projector places every name the index grammar declares, and places
    #    it in a real column. Every index name is part of a fixing's identity,
    #    because ORE writes it into the index name, so a name with no column
    #    would let two fixings project alike. A name the projector places that
    #    no family declares is the same failure the other way: the mapping and
    #    the grammar have parted company.
    for missing in sorted(index_names - set(index_placed)):
        problems.append(
            f"the projector places no column for index name '{missing}', "
            "which an index family declares."
        )
    for extra in sorted(set(index_placed) - index_names):
        problems.append(
            f"the projector places index name '{extra}', which no index family declares."
        )
    for name, column in sorted(index_placed.items()):
        if column not in columns:
            problems.append(
                f"the projector places index name '{name}' in column '{column}', "
                f"which {CREATE_TABLE} does not have."
            )

    if problems:
        return fail(problems)
    print(
        f"OK: {len(identity)} identity fields, {len(columns)} columns, "
        f"{len(placed)} switch cases, {len(found)} asset-class views and "
        f"{len(index_names)} index names agree with the models."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
