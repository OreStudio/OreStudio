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

INCLUDE_RE = re.compile(r"^\s*\\(i(?:r)?)\s+(\S+)", re.M)


def reached_files() -> tuple[set[Path], list[tuple[Path, str, Path]]]:
    """Every file psql runs, and every include that names no file.

    An include whose target does not exist is a failure in its own right: psql
    aborts the whole build on it, and a check that skipped it would report a
    clean tree for exactly the case that stops the build.
    """
    seen: set[Path] = set()
    missing: list[tuple[Path, str, Path]] = []
    stack = sorted(SQL_ROOT.glob("*.sql"))
    while stack:
        path = stack.pop()
        if path in seen:
            continue
        seen.add(path)
        text = path.read_text(errors="ignore")
        for form, entry in INCLUDE_RE.findall(text):
            # \ir resolves against the including file; \i against psql's
            # working directory, which is the schema root.
            base = path.parent if form == "ir" else SQL_ROOT
            target = (base / entry).resolve()
            if target.exists():
                stack.append(target)
            else:
                missing.append((path, entry, target))
    return seen, missing


def check() -> tuple[list[Path], list[tuple[Path, str, Path]]]:
    reached, missing = reached_files()
    orphans = [
        path
        for path in sorted(CREATE_DIR.glob("**/*.sql"))
        if path.resolve() not in reached
    ]
    return orphans, missing


def main():
    orphans, missing = check()

    for path, entry, target in missing:
        print(
            f"{path.relative_to(REPO_ROOT)}: includes {entry}, which does not "
            f"exist (looked for {target.relative_to(REPO_ROOT)})"
        )
    for path in orphans:
        print(
            f"{path.relative_to(REPO_ROOT)}: no schema entry point includes "
            f"this file, so psql never runs it"
        )

    if missing:
        print(
            f"\n{len(missing)} include(s) name a file that is not there. psql "
            f"aborts on the first of them."
        )
    if orphans:
        print(
            f"\n{len(orphans)} unreachable create file(s). Add an include to "
            f"the component's aggregator, in dependency order."
        )
    if missing or orphans:
        return 1
    print("every create file is reachable from a schema entry point.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
