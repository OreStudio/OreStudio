#!/usr/bin/env python3
"""A column's SQL type in the model is the type the table is created with.

`check_model_drift` compares structure, not types. While the models said
`numeric(38, 12)` and the generated SQL still said `numeric(28, 12)` -- a
regeneration that had not reached the component -- it passed. A precision or
scale change is therefore invisible to every existing check and is confirmed
only by a build and a database recreate, which is expensive enough that it was
tempting to believe the gate instead.

This check reads both sides and compares them: the model's `** <column>` drawer
under `* Columns`, and the `create table` statement the generator emits for the
`:tablename:` the model declares. Only columns present on both sides are
compared, because the generated table carries scaffolding (version, tenant_id,
valid_from, ...) that the model does not name as a column.

The check reads the tree and writes nothing.

Usage:
  check_model_sql_types.py [--root .]
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

TABLENAME = re.compile(r"^\s*:tablename:\s*(\S+)\s*$", re.MULTILINE)
COLUMN = re.compile(r"^\*\* (\S+)\s*$")
SECTION = re.compile(r"^\* [^*\s].*$")
TYPE = re.compile(r"^\s*:type:\s*(\S.*?)\s*$")
CREATE_COLUMN = re.compile(r'^\s*"([A-Za-z0-9_]+)"\s+(.+?)\s*,?\s*$')
CREATE_START = re.compile(r"^\s*create\s+table\s", re.IGNORECASE | re.MULTILINE)

# A generated column definition is the type followed by its constraints. The
# model states the type alone, so everything from the first constraint boundary
# on is dropped before the two are compared. `with time zone` and `double
# precision` are part of the type and are not in this list.
CONSTRAINT = re.compile(
    r"\s+(not\s+null|null|default|primary\s+key|references|check|unique|"
    r"generated|collate|constraint)\b",
    re.IGNORECASE)

# A run that reads implausibly few columns has stopped understanding the
# models and would pass vacuously. The tree holds several thousand, so the
# floor sits far below that and far above what a broken parse returns.
MIN_COMPARED = 500


def normalise(sql_type: str) -> str:
    """Collapse whitespace so `numeric(38, 12)` matches `numeric(38,12)`."""
    return re.sub(r"\s+", " ", sql_type.strip().lower())


def model_columns(text: str) -> dict[str, str]:
    """The `* Columns` section: column heading -> declared SQL type."""
    columns: dict[str, str] = {}
    in_columns = False
    name: str | None = None
    for raw in text.splitlines():
        if SECTION.match(raw):
            heading = raw.strip()
            if heading == "* Columns":
                in_columns = True
                name = None
                continue
            if in_columns:
                break
            continue
        if not in_columns:
            continue
        m = COLUMN.match(raw)
        if m:
            name = m.group(1)
            continue
        t = TYPE.match(raw)
        if t and name is not None and name not in columns:
            columns[name] = t.group(1)
    return columns


def created_columns(text: str) -> dict[str, str]:
    """The body of the first `create table`: quoted column -> SQL type."""
    columns: dict[str, str] = {}
    inside = False
    for raw in text.splitlines():
        if not inside:
            if CREATE_START.match(raw):
                inside = True
            continue
        stripped = raw.strip()
        if stripped.startswith(")"):
            break
        if not stripped or stripped.startswith("--"):
            continue
        m = CREATE_COLUMN.match(raw)
        if m:
            definition = CONSTRAINT.split(m.group(2), maxsplit=1)[0]
            columns[m.group(1)] = definition.strip()
    return columns


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path("."))
    parser.add_argument(
        "--min-compared", type=int, default=MIN_COMPARED,
        help="vacuity floor; lower it only when running against a subtree")
    args = parser.parse_args()
    root = args.root

    # Index the generator's output by the table each file actually creates.
    # The file name is not the table name -- `ores_workflow_plan_dependencies_tbl`
    # lives in `workflow_workflow_plan_dependencies_create.sql` -- so deriving
    # one from the other silently drops 30 tables and reports them as missing.
    create_files: dict[str, Path] = {}
    for path in sorted(root.glob("projects/ores.sql/create/**/*_create.sql")):
        sql_text = path.read_text(encoding="utf-8")
        start = CREATE_START.search(sql_text)
        if not start:
            continue
        tail = sql_text[start.end():]
        name = re.match(r'\s*(?:if\s+not\s+exists\s+)?"([A-Za-z0-9_]+)"', tail,
                        re.IGNORECASE)
        if name:
            create_files[name.group(1)] = path

    def create_file_for(table: str) -> Path | None:
        return create_files.get(table)

    compared = 0
    missing_sql: list[tuple[Path, str]] = []
    mismatches: list[tuple[Path, str, str, str, Path]] = []

    for path in sorted(root.glob("projects/**/modeling/*.org")):
        text = path.read_text(encoding="utf-8")
        m = TABLENAME.search(text)
        if not m:
            continue
        table = m.group(1)
        sql_path = create_file_for(table)
        if sql_path is None:
            missing_sql.append((path, table))
            continue
        declared = model_columns(text)
        if not declared:
            continue
        actual = created_columns(sql_path.read_text(encoding="utf-8"))
        for name, model_type in declared.items():
            if name not in actual:
                continue
            compared += 1
            if normalise(model_type) != normalise(actual[name]):
                mismatches.append((path, name, model_type, actual[name], sql_path))

    for path, table in missing_sql:
        print(f"{path.as_posix()}: no create table for {table}")

    for path, name, model_type, sql_type, sql_path in mismatches:
        print(f"{path.as_posix()}:{name}: model {model_type!r} "
              f"but {sql_path.as_posix()} creates {sql_type!r}")

    print(f"\nmodels with a tablename and no matching create: {len(missing_sql)}")
    print(f"columns compared: {compared} (floor {MIN_COMPARED})")
    print(f"type mismatches: {len(mismatches)}")

    if compared < args.min_compared:
        print(f"ERROR: compared only {compared} columns, expected at least "
              f"{args.min_compared}. The model or SQL format may have changed, "
              f"and this check would otherwise pass without looking at anything.")
        return 1
    if mismatches:
        print("\nThe generated SQL disagrees with the model. Regenerate the "
              "component, then rebuild and recreate.")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
