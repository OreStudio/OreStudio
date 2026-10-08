#!/usr/bin/env python3
"""Check that every SQL file is reachable from a schema entry point.

projects/ores.sql is three dependency graphs -- create/, populate/ and drop/
-- each carried in psql include directives. Every component has an aggregator, such as
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
POPULATE_DIR = SQL_ROOT / "populate"
DROP_DIR = SQL_ROOT / "drop"

# Every tree that psql walks by include, and the directory each one holds.
TREES = {
    "create": CREATE_DIR,
    "populate": POPULATE_DIR,
    "drop": DROP_DIR,
}

# Files psql runs that nothing else includes, so the walk has to start at them
# as well as at the root-level files. drop/drop.sql is the drop tree's
# aggregator and no other file names it; the other two are run by hand, once,
# when an operator wants that data.
EXTRA_ENTRY_POINTS = (
    "drop/drop.sql",
    # These drop a whole database rather than a table. Nothing includes them,
    # because they are what an operator runs.
    "drop/drop_database.sql",
    "drop/drop_test_databases.sql",
    # Reads a TSV the operator downloads from iptoasn.com through a psql
    # variable, and truncates and reloads to refresh. It takes a file path, so
    # it cannot be a DQ dataset.
    "populate/ip2country/ip2country_import.sql",
)

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
    for name in EXTRA_ENTRY_POINTS:
        path = SQL_ROOT / name
        if path.exists():
            stack.append(path)
        else:
            # A declared entry point that has been renamed or deleted is the
            # same failure as an include that names nothing.
            missing.append((SQL_ROOT, name, path))
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
    orphans: list[Path] = []
    for tree, directory in TREES.items():
        for path in sorted(directory.glob("**/*.sql")):
            if path.resolve() not in reached:
                orphans.append(path)
    return orphans, missing


def main():
    reached, missing = reached_files()

    for path, entry, target in missing:
        print(
            f"{path.relative_to(REPO_ROOT)}: includes {entry}, which does not "
            f"exist (looked for {target.relative_to(REPO_ROOT)})"
        )

    total = 0
    for tree, directory in TREES.items():
        tree_orphans = [
            path
            for path in sorted(directory.glob("**/*.sql"))
            if path.resolve() not in reached
        ]
        if not tree_orphans:
            continue
        print(f"\n{tree}/: {len(tree_orphans)} file(s) nothing includes:")
        for path in tree_orphans:
            print(f"  {path.relative_to(REPO_ROOT)}")
        total += len(tree_orphans)

    if missing:
        print(
            f"\n{len(missing)} include(s) name a file that is not there. psql "
            f"aborts on the first of them."
        )
    if total:
        print(
            f"\n{total} unreachable file(s). Add an include to the tree's "
            f"aggregator in dependency order, or declare the file an entry "
            f"point if it is run on its own."
        )
    if missing or total:
        return 1
    print(
        "every file in create/, populate/ and drop/ is reached by a schema "
        "entry point."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
