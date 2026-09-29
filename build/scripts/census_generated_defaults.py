#!/usr/bin/env python3
"""Count the model columns that generate an indeterminate C++ member.

A column whose cpp_type is a plain arithmetic or chrono value type and that
declares no :default_value: leaves its generated member with no in-class
initialiser. A column that is nullable but whose cpp_type is a bare value type
is the same trap from the other side: nullability is stated and the C++ type
cannot carry it. Codegen synthesises a default for bool and int alone, so the
columns this prints are the ones whose member is genuinely indeterminate.

The valgrind hotfix filed the non-nullable remainder as a backlog capture with
this census behind it:

    python3 build/scripts/census_generated_defaults.py
"""

from __future__ import annotations

import collections
import pathlib
import re

HEADING_RE = re.compile(r"^\*\* (\S+)\s*$")
PROPERTY_RE = re.compile(r"^:([a-z_]+):\s*(.*?)\s*$")

ARITHMETIC = {
    "double",
    "float",
    "int",
    "std::int64_t",
    "std::uint64_t",
    "std::uint32_t",
    "std::uint16_t",
    "std::int32_t",
    "std::int16_t",
}
CHRONO = {"std::chrono::year_month_day", "std::chrono::system_clock::time_point"}
INDETERMINATE = ARITHMETIC | CHRONO


def columns(path: pathlib.Path):
    name = None
    drawer: dict[str, str] = {}
    in_properties = False
    for line in path.read_text(errors="ignore").splitlines():
        heading = HEADING_RE.match(line)
        if heading:
            if name is not None:
                yield name, drawer
            name, drawer, in_properties = heading.group(1), {}, False
            continue
        if line.strip() == ":PROPERTIES:":
            in_properties = True
            continue
        if line.strip() == ":END:":
            in_properties = False
            continue
        if in_properties:
            prop = PROPERTY_RE.match(line)
            if prop:
                drawer[prop.group(1)] = prop.group(2)
    if name is not None:
        yield name, drawer


def main() -> None:
    missing: dict[str, list[str]] = collections.defaultdict(list)
    nullable_bare: dict[str, list[str]] = collections.defaultdict(list)
    by_type = collections.Counter()
    for path in sorted(pathlib.Path("projects").glob("*/modeling/*.org")):
        for name, drawer in columns(path):
            cpp_type = drawer.get("cpp_type")
            if cpp_type not in INDETERMINATE:
                continue
            nullable = drawer.get("nullable") == "true"
            default = drawer.get("default_value")
            if nullable and not cpp_type.startswith("std::optional"):
                nullable_bare[str(path)].append(f"{name}: {cpp_type}")
            if not default and not nullable:
                missing[str(path)].append(f"{name}: {cpp_type}")
                by_type[cpp_type] += 1
    print("non-nullable indeterminate columns with no :default_value:")
    for path, names in sorted(missing.items()):
        print(f"  {path} ({len(names)})")
        for entry in names:
            print(f"      {entry}")
    print("\nby type:", dict(by_type))
    print("\ntotal columns:", sum(by_type.values()), "in", len(missing), "files")
    print("\nnullable columns whose cpp_type is a bare value type:")
    for path, names in sorted(nullable_bare.items()):
        print(f"  {path} ({len(names)}): {', '.join(names)}")


if __name__ == "__main__":
    main()
