#!/usr/bin/env python3
"""A row binds a column in the format the column's own type defines.

A generated repository prepares its statement without naming the types of its
parameters, so the server infers each one from where its placeholder sits: the
column's own type. The member the row binds is written in the format its codec
defines, and the two must be the same type. A member that disagrees is accepted
by the compiler, because both sides are bytes by the time the statement runs,
and refused by the server with

    incorrect binary data format in bind parameter N

which names neither the column nor the type. `payment_lag` was an
`std::optional<std::int64_t>` on an `integer` column and was found only by the
suite that wrote a row.

A column the row holds as text is not compared: it is bound as text, the server
parses it, and the compiler proves the text form exists.

The check reads the generated entity headers against the tables the generated
SQL creates, because the row's type is what the generator emits and not what
the model states -- an `inet` column's domain member is an address while the
row keeps a string. It writes nothing.

Usage:
  check_entity_binary_types.py [--root .] [--min-compared 500]
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path
from typing import NamedTuple

REPO_ROOT = Path(__file__).resolve().parents[3]

MARKER = "AUTO-GENERATED FILE"

TABLENAME = re.compile(r'constexpr static const char\* tablename = "(\w+)"')

# A member of the row struct. The type has no parentheses and the initialiser
# may hold them, so the type is read up to the member name.
MEMBER = re.compile(
    r"^ {4}([A-Za-z_][A-Za-z0-9_:<>, ]*?) +([a-z_][A-Za-z0-9_]*)"
    r"(?: *= *[^;]*)?;$",
    re.MULTILINE)

BINDING = re.compile(
    r"query_builder::(bind|bind_text|key|key_text)\("
    r"\"([a-z0-9_]+)\" *, *&e::([a-z_][a-z0-9_]*) *\)")

CREATE_TABLE = re.compile(
    r"create +table +(?:if +not +exists +)?\"?([a-z0-9_]+)\"? *\((.*?)\) *;",
    re.IGNORECASE | re.DOTALL)

# A view is read by an entity row too, but its column types are the types of a
# select list this check does not evaluate, so a view is left to the suite that
# reads it.
CREATE_VIEW = re.compile(
    r"create +(?:or +replace +)?view +\"?([a-z0-9_]+)\"?",
    re.IGNORECASE)

# The generated column definition is the quoted name and its type, then its
# constraints. `with time zone` and `double precision` are part of the type and
# are not constraints.
CREATE_COLUMN = re.compile(r'^ *,? *"([a-z0-9_]+)" +(.+)$', re.IGNORECASE)
CONSTRAINT = re.compile(
    r" +(?:not +null|null|default|primary +key|references|check|unique|"
    r"generated|collate|constraint)\b",
    re.IGNORECASE)

# The format the codec of a member writes, as the type class and, for the
# integers, the width the type occupies on the wire.
CPP_WIRE: dict[str, tuple[str, int | None]] = {
    "bool": ("boolean", None),
    "std::string": ("text", None),
    "std::string_view": ("text", None),
    "boost::uuids::uuid": ("uuid", None),
    "utility::uuid::tenant_id": ("uuid", None),
    "std::chrono::system_clock::time_point": ("timestamptz", None),
    "db_timestamp": ("timestamptz", None),
    "std::chrono::year_month_day": ("date", None),
    "ores::utility::decimal::decimal": ("numeric", None),
    "decimal": ("numeric", None),
    "double": ("double precision", None),
    "float": ("real", None),
    "int": ("integer", 32),
    "std::int32_t": ("integer", 32),
    "std::uint32_t": ("integer", 32),
    "std::int64_t": ("integer", 64),
    "std::uint64_t": ("integer", 64),
    "std::size_t": ("integer", 64),
    "long": ("integer", 64),
    "long long": ("integer", 64),
    "std::int16_t": ("integer", 16),
    "std::uint16_t": ("integer", 16),
    "short": ("integer", 16),
}

SQL_WIRE: dict[str, tuple[str, int | None]] = {
    "text": ("text", None),
    "character varying": ("text", None),
    "varchar": ("text", None),
    "uuid": ("uuid", None),
    "boolean": ("boolean", None),
    "bool": ("boolean", None),
    "date": ("date", None),
    "timestamp with time zone": ("timestamptz", None),
    "timestamp without time zone": ("timestamp", None),
    "timestamp": ("timestamp", None),
    "double precision": ("double precision", None),
    "real": ("real", None),
    "inet": ("inet", None),
    "jsonb": ("jsonb", None),
    "integer": ("integer", 32),
    "int": ("integer", 32),
    "int4": ("integer", 32),
    "bigint": ("integer", 64),
    "int8": ("integer", 64),
    "smallint": ("integer", 16),
    "int2": ("integer", 16),
}

# A run that compares implausibly few columns has stopped understanding the
# tree and would pass vacuously.
MIN_COMPARED = 500


def normalise_space(text: str) -> str:
    return re.sub(r"\s+", " ", text)


def wire_of_cpp(cpp_type: str) -> tuple[str, int | None] | None:
    """The format a member's codec writes, from the member's declared type."""
    t = cpp_type.strip()
    if t.startswith("std::optional<") and t.endswith(">"):
        t = t[len("std::optional<") : -1].strip()
    if t.startswith("const "):
        t = t[len("const ") :]
    # An array travels as its element's format behind the array's header, so
    # the element is what has to match the column.
    if t.startswith("std::vector<") and t.endswith(">"):
        t = t[len("std::vector<") : -1].strip()
    return CPP_WIRE.get(t)


def wire_of_sql(sql_type: str) -> tuple[str, int | None] | None:
    """The format a column's type defines, or None when it is not a type here."""
    t = normalise_space(sql_type).strip().lower().rstrip(", ")
    # An array column's values are read one element at a time, each in its
    # element's own format.
    if t.endswith("[]"):
        t = t[: -len("[]")].strip()
    if t.startswith("numeric"):
        return ("numeric", None)
    sized = re.match(r"^(?:character varying|varchar|character|char)\s*\(\s*\d+\s*\)$", t)
    if sized:
        return ("text", None)
    if t in SQL_WIRE:
        return SQL_WIRE[t]
    return None


def row_members(text: str) -> dict[str, str]:
    """Member name -> declared type, for the members of the row struct."""
    members: dict[str, str] = {}
    for m in MEMBER.finditer(text):
        cpp_type, name = m.group(1).strip(), m.group(2)
        if cpp_type.startswith(("constexpr", "static", "using", "return", "struct")):
            continue
        members.setdefault(name, cpp_type)
    return members


def bindings(text: str) -> list[tuple[str, str, bool]]:
    """(column, member, is_binary) in the order the row states them."""
    out = []
    for kind, column, member in BINDING.findall(normalise_space(text)):
        out.append((column, member, kind in ("bind", "key")))
    return out


def created_columns(root: Path) -> tuple[dict[str, dict[str, str]], set[str]]:
    """Table name -> column name -> declared SQL type, and the views' names."""
    tables: dict[str, dict[str, str]] = {}
    views: set[str] = set()
    for sql_file in sorted(root.glob("projects/ores.sql/create/*/*_create.sql")):
        text = sql_file.read_text(encoding="utf-8", errors="replace")
        views.update(CREATE_VIEW.findall(text))
        for table, body in CREATE_TABLE.findall(text):
            columns = tables.setdefault(table, {})
            for line in body.splitlines():
                m = CREATE_COLUMN.match(line)
                if not m:
                    continue
                constraint = CONSTRAINT.search(m.group(2))
                declared = m.group(2)[: constraint.start()] if constraint else m.group(2)
                columns.setdefault(m.group(1), normalise_space(declared).strip().rstrip(", "))
    return tables, views


def entity_headers(root: Path) -> list[Path]:
    headers = []
    for path in sorted(root.glob("projects/**/repository/*_entity.hpp")):
        text = path.read_text(encoding="utf-8", errors="replace")
        if MARKER in text and "static constexpr auto columns()" in text:
            headers.append(path)
    return headers


class Result(NamedTuple):
    compared: int
    skipped: int
    unchecked: int
    mismatches: list[str]
    unclassified: list[str]
    no_table: list[str]

    @property
    def failed(self) -> bool:
        return bool(self.mismatches or self.unclassified or self.no_table)


def compare(root: Path) -> Result:
    """The disagreement between each row's members and its table's columns."""
    tables, views = created_columns(root)

    compared = 0
    skipped = 0
    unchecked = 0
    mismatches: list[str] = []
    unclassified: list[str] = []
    no_table: list[str] = []

    for header in entity_headers(root):
        text = header.read_text(encoding="utf-8", errors="replace")
        found_table = TABLENAME.search(text)
        if not found_table:
            continue
        table = found_table.group(1)
        if table in views:
            continue
        if table not in tables:
            no_table.append(f"{header.relative_to(root)}: no create script creates {table}")
            continue
        columns = tables[table]
        members = row_members(text)
        for column, member, is_binary in bindings(text):
            if not is_binary:
                skipped += 1
                continue
            if column not in columns:
                mismatches.append(
                    f"{header.relative_to(root)}: the row binds '{column}', which "
                    f"{table} does not create")
                continue
            cpp_type = members.get(member)
            if cpp_type is None:
                mismatches.append(
                    f"{header.relative_to(root)}: the row binds '{member}' but the "
                    f"member is not declared in the struct")
                continue
            cpp_wire = wire_of_cpp(cpp_type)
            sql_wire = wire_of_sql(columns[column])
            if cpp_wire is None:
                unclassified.append(
                    f"{header.relative_to(root)}: '{column}' holds {cpp_type}, which "
                    f"this check cannot place on the wire")
                continue
            if sql_wire is None:
                # A type this check cannot place on the wire is consistent with
                # the row only when the row keeps the text, which is the form a
                # custom enum or a domain type takes. Anything else is a type
                # the table states and the row guesses at.
                if cpp_wire == ("text", None):
                    unchecked += 1
                    continue
                unclassified.append(
                    f"{header.relative_to(root)}: '{column}' holds {cpp_type} but "
                    f"{table} declares it {columns[column]}, a type this check "
                    f"cannot place on the wire")
                continue
            compared += 1
            if cpp_wire != sql_wire:
                mismatches.append(
                    f"{header.relative_to(root)}: '{column}' holds {cpp_type} "
                    f"({cpp_wire[0]}{'' if cpp_wire[1] is None else ' ' + str(cpp_wire[1])}) "
                    f"but {table} creates it {columns[column]} "
                    f"({sql_wire[0]}{'' if sql_wire[1] is None else ' ' + str(sql_wire[1])})")

    return Result(compared, skipped, unchecked, mismatches, unclassified, no_table)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=str(REPO_ROOT))
    parser.add_argument("--min-compared", type=int, default=MIN_COMPARED)
    args = parser.parse_args()

    result = compare(Path(args.root).resolve())
    for line in result.no_table:
        print(f"NO TABLE: {line}")
    for line in result.unclassified:
        print(f"UNCLASSIFIED: {line}")
    for line in result.mismatches:
        print(f"MISMATCH: {line}")

    print(f"{result.compared} binary columns compared, {result.skipped} text columns "
          f"skipped, {result.unchecked} read as text by a type this check does not "
          f"place, {len(result.mismatches)} mismatches, "
          f"{len(result.unclassified)} unclassified, "
          f"{len(result.no_table)} tables with no create script")

    if result.failed:
        return 1
    if result.compared < args.min_compared:
        print(f"The check compared {result.compared} columns, below the floor of "
              f"{args.min_compared}: it is no longer reading the tree.")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
