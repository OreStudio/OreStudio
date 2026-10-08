#!/usr/bin/env python3
"""Check that generated code names no column its entity does not have.

A codegen entity model declares its columns in `* Columns`, but two sections
that reach generated code are free text: the C++ `** Table display` table and
the SQL `** Indexes` table. A column removed from `* Columns` and left behind
in either one still regenerates faithfully, so `check_model_drift` stays green
and the disagreement reaches `main` -- where the compiler or a database
recreate finds it, and only after the branch has merged.

Two rules, both read from the generated output rather than the model, because
the generated class carries the flag-derived columns (version, tenant_id,
modified_by, and the rest) that the model does not name:

  1. Every `x.member` a generated table writer streams is a member of the
     generated domain class for that entity.
  2. Every column an index names is a column of the table the same file
     creates. An index on a missing column fails, so the table cannot be
     created at all.

The check reads the tree and writes nothing.

Usage:
  check_generated_column_refs.py
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]

MARKER = "AUTO-GENERATED FILE"

COMMENT = re.compile(r"/\*.*?\*/|//[^\n]*", re.DOTALL)

# The writer's loop, whose iterator names the entity being streamed. The
# container is not matched: only the binding matters.
ITERATOR = re.compile(r"const\s+auto&\s+(\w+)\s*:")

ACCESS = re.compile(r"\b(\w+)\.(\w+)\b")

CREATE_TABLE = re.compile(
    r"create\s+table\s+(?:if\s+not\s+exists\s+)?\"?(ores_\w+_tbl)\"?\s*\((.*?)\n\)\s*;",
    re.IGNORECASE | re.DOTALL,
)

# A leading comma before the column is the generator's own shape for the last
# column of an entity with no nullable tail.
COLUMN_DEF = re.compile(
    r"^\s*,?\s*\"([a-z_][a-z0-9_]*)\"\s+[a-z]", re.IGNORECASE | re.MULTILINE
)

CREATE_INDEX = re.compile(
    r"create\s+(?:unique\s+)?index\s+(?:if\s+not\s+exists\s+)?\"?(\w+)\"?\s+"
    r"on\s+\"?(ores_\w+_tbl)\"?\s*\((.*?)\)",
    re.IGNORECASE | re.DOTALL,
)

INDEX_COLUMN = re.compile(r"\"?([a-z_][a-z0-9_]*)\"?")


def strip_comments(text: str) -> str:
    """``text`` without comments, so a name in prose is not read as a use."""
    return COMMENT.sub(" ", text)


def table_writer_problems() -> list[str]:
    """Rule 1: what a generated table writer streams must be a member."""
    problems: list[str] = []
    for cpp in sorted(REPO_ROOT.glob("projects/*/api/src/domain/*_table.cpp")):
        text = cpp.read_text()
        if MARKER not in text[:1500]:
            continue
        rel = cpp.relative_to(REPO_ROOT)
        body = strip_comments(text)

        binding = ITERATOR.search(body)
        if not binding:
            problems.append(f"{rel}: no `const auto& x : v` loop to check")
            continue
        var = binding.group(1)

        entity = cpp.name[: -len("_table.cpp")]
        headers = list(
            cpp.parents[2].glob(f"include/*/domain/{entity}.hpp")
        )
        if len(headers) != 1:
            problems.append(
                f"{rel}: {len(headers)} headers match {entity}.hpp, expected 1"
            )
            continue
        members = set(re.findall(r"\w+", strip_comments(headers[0].read_text())))

        for access in ACCESS.finditer(body):
            if access.group(1) == var and access.group(2) not in members:
                problems.append(
                    f"{rel}: `{var}.{access.group(2)}` is not a member of {entity}"
                )
    return problems


def sql_index_problems() -> list[str]:
    """Rule 2: an index may only name a column of the table it indexes."""
    problems: list[str] = []
    for sql in sorted(REPO_ROOT.glob("projects/ores.sql/create/**/*.sql")):
        text = sql.read_text()
        if MARKER not in text[:1500]:
            continue
        rel = sql.relative_to(REPO_ROOT)

        columns = {
            table.lower(): {c.lower() for c in COLUMN_DEF.findall(body)}
            for table, body in CREATE_TABLE.findall(text)
        }
        for name, table, cols in CREATE_INDEX.findall(text):
            known = columns.get(table.lower())
            if known is None:
                problems.append(f"{rel}: index {name} on unknown table {table}")
                continue
            for raw in cols.split(","):
                column = INDEX_COLUMN.search(raw.strip())
                if column and column.group(1).lower() not in known:
                    problems.append(
                        f"{rel}: index {name} names {column.group(1)}, "
                        f"which is not a column of {table}"
                    )
    return problems


def main() -> int:
    problems = table_writer_problems() + sql_index_problems()
    for problem in problems:
        print(problem)
    if problems:
        print(f"{len(problems)} problem(s)")
        return 1
    print("every generated column reference names a column that exists")
    return 0


if __name__ == "__main__":
    sys.exit(main())
