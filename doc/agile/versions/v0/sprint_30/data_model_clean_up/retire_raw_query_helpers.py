#!/usr/bin/env python3
"""Move every caller of the raw query helpers onto ores::database::query_builder.

Each call, however it is qualified, becomes the fully qualified query_builder
name; a using-declaration of a helper is dropped; a file that now calls
query_builder includes its header; an include of the retired
bitemporal_operations header becomes the query_builder include, with the
repository helpers where the file uses them. The script is idempotent: a second run
changes nothing.

Usage: retire_raw_query_helpers.py [repository root]
"""
import pathlib
import re
import sys

NAMES = (
    "execute_raw_string_query",
    "execute_raw_multi_column_query",
    "execute_raw_command",
    "execute_parameterized_string_query",
    "execute_parameterized_command",
    "execute_parameterized_multi_column_query",
)
NAME = "(" + "|".join(NAMES) + r")\b"
TARGET = "ores::database::query_builder::"
INCLUDE = '#include "ores.database/query_builder/execute.hpp"'
RETIRED = '#include "ores.database/repository/bitemporal_operations.hpp"'
HELPERS = '#include "ores.database/repository/helpers.hpp"'
HELPERS_USE = re.compile(r"\b(MAX_TIMESTAMP|ensure_success|db_timestamp)\b")

# A call already qualified with query_builder, through the namespace alias a
# generated repository declares, stays as it is: the codegen models and
# templates spell it that way, and the generated file must match them.
QUALIFIER = r"(?:(?:ores::)?database::repository::|repository::|db::)?"
CALL = re.compile(r"(?<![\w:])" + QUALIFIER + NAME)
USING = re.compile(
    r"^[ \t]*using (?:ores::)?database::(?:repository|query_builder)::" + NAME + r";[ \t]*\n",
    re.M,
)
SKIP = (
    "projects/ores.database/src/repository/bitemporal_operations.cpp",
    "projects/ores.database/include/ores.database/repository/bitemporal_operations.hpp",
    "projects/ores.database/src/query_builder/query.cpp",
    "projects/ores.database/include/ores.database/query_builder/execute.hpp",
)


def add_include(text: str, include: str = INCLUDE) -> str:
    if include in text:
        return text
    lines = text.split("\n")
    quoted = [i for i, l in enumerate(lines) if l.startswith('#include "')]
    if not quoted:
        quoted = [i for i, l in enumerate(lines) if l.startswith("#include ")] or [-1]
        lines.insert(quoted[0] + 1, include)
        return "\n".join(lines)
    # A source file's first include is its own header, which stays first.
    rest = quoted[1:] if len(quoted) > 1 else quoted
    at = next((i for i in rest if lines[i] > include), quoted[-1] + 1)
    lines.insert(at, include)
    return "\n".join(lines)


def drop_retired_header(text: str) -> str:
    """The retired header gave its includers context, the unit of work and the
    repository helpers; each includer gets what it uses."""
    if RETIRED not in text:
        return text
    out = re.sub(r"^" + re.escape(RETIRED) + r"[ \t]*\n", "", text, flags=re.M)
    out = add_include(out)
    if HELPERS_USE.search(out):
        out = add_include(out, HELPERS)
    return out


def migrate(text: str) -> str:
    out = USING.sub("", text)
    out = CALL.sub(lambda m: TARGET + m.group(1), out)
    if out != text:
        out = add_include(out)
    return drop_retired_header(out)


def main() -> int:
    root = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else ".").resolve()
    changed = 0
    for path in sorted((root / "projects").rglob("*")):
        if path.suffix not in (".cpp", ".hpp") or not path.is_file():
            continue
        rel = path.relative_to(root).as_posix()
        if rel in SKIP or "/build/" in rel:
            continue
        text = path.read_text()
        out = migrate(text)
        if out != text:
            path.write_text(out)
            changed += 1
            print(rel)
    print(f"{changed} files changed", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
