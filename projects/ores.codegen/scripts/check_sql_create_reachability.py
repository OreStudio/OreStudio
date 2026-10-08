#!/usr/bin/env python3
"""Check that every create file is reachable from a schema entry point.

projects/ores.sql/create/ is a dependency graph carried in psql include
directives. Every component has an aggregator, such as
create/trading/trading_create.sql, that names its tables in dependency order,
and create/create.sql names the aggregators. setup_schema.sql and
recreate_database.sql are the entry points that psql runs.

A generated create file that no aggregator names is never executed, so the
table it declares does not exist in any database. Nothing else notices: the
C++ compiles against the model, the codegen drift check sees a file that
matches its model, and the populate check sees a table the scripts agree on.
The first symptom is an error, hundreds of files later, in whichever file
happens to reference the missing table.

Both include forms count. ``\\ir`` resolves relative to the including file,
which the aggregators use; ``\\i`` resolves against psql's working directory,
which the top-level entry points use.

Run::

    python3 projects/ores.codegen/scripts/check_sql_create_reachability.py
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SQL_ROOT = REPO_ROOT / "projects" / "ores.sql"
CREATE_DIR = SQL_ROOT / "create"

INCLUDE_RE = re.compile(r"^\s*\\i(?:r)?\s+(\S+)", re.M)


def reached_files() -> set[Path]:
    """Every file psql runs, starting from the top-level entry points."""
    seen: set[Path] = set()
    stack = sorted(SQL_ROOT.glob("*.sql"))
    while stack:
        path = stack.pop()
        if path in seen or not path.exists():
            continue
        seen.add(path)
        text = path.read_text(errors="ignore")
        for entry in INCLUDE_RE.findall(text):
            stack.append((path.parent / entry).resolve())
    return seen


def check() -> list[Path]:
    reached = reached_files()
    orphans = []
    for path in sorted(CREATE_DIR.glob("**/*.sql")):
        resolved = path.resolve()
        if resolved not in reached:
            orphans.append(path)
    return orphans


def main():
    orphans = check()
    for path in orphans:
        print(
            f"{path.relative_to(REPO_ROOT)}: no schema entry point includes "
            f"this file, so psql never runs it"
        )
    if orphans:
        print(
            f"\n{len(orphans)} unreachable create file(s). Add an include to "
            f"the component's aggregator, in dependency order."
        )
        return 1
    print("every create file is reachable from a schema entry point.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
