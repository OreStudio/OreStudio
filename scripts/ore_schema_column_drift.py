#!/usr/bin/env python3
"""Report columns the create scripts declare that the database does not have.

A restored database carries the schema of the commit it was restored from.
Re-running the create aggregate repairs the tables a later commit added,
because each script creates its table when the table is absent. It cannot
repair a column a later commit added to a table that already existed:
`create table if not exists` skips the table and every column in it, and
reports no error.

The gap stays invisible until something writes the column, and the failure
then names a missing column or a violated check constraint rather than the
drift. This script reads the column list out of every create script and
diffs it against information_schema, so the gap shows up before a test run
costs a build and a suite.

Exits 1 when a column is missing, 0 otherwise.

  ./scripts/ore_schema_column_drift.py
  ./scripts/ore_schema_column_drift.py --component trading
  ./scripts/ore_schema_column_drift.py --emit-alters > /tmp/apply.sql
"""
import argparse
import glob
import os
import pathlib
import re
import subprocess
import sys

CREATE_GLOB = "projects/ores.sql/create/**/*_create.sql"
TABLE_RE = re.compile(
    r'create table if not exists "([^"]+)"\s*\((.*?)\n\)\s*;', re.S | re.I)
COLUMN_RE = re.compile(r'^\s*"([^"]+)"\s+(\S.*)$', re.M)


def read_env(path=".env"):
    p = pathlib.Path(path)
    if not p.exists():
        sys.exit(f"{path} not found; run from the worktree root.")
    out = {}
    for line in p.read_text().splitlines():
        line = line.strip()
        if line and not line.startswith("#") and "=" in line:
            k, _, v = line.partition("=")
            out[k.strip()] = v.strip().strip('"').strip("'")
    return out


def make_sql(env):
    db = env.get("ORES_TEST_DB_DATABASE")
    if not db:
        sys.exit("No ORES_TEST_DB_DATABASE in .env.")
    argv = ["psql", "-At", "-F", "\x1f", "-q", "-v", "ON_ERROR_STOP=1",
            "-h", env.get("PGHOST", "localhost"),
            "-p", env.get("PGPORT", "5432"),
            "-U", env.get("PGUSER", "postgres"), "-d", db]
    ce = os.environ.copy()
    if env.get("PGPASSWORD"):
        ce["PGPASSWORD"] = env["PGPASSWORD"]

    def sql(stmt):
        r = subprocess.run(argv + ["-c", stmt], capture_output=True,
                           text=True, env=ce)
        if r.returncode != 0:
            raise RuntimeError(r.stderr.strip())
        return r
    return sql


def declared_columns(component=None):
    """Table to ordered {column: definition} as the create scripts declare it."""
    pattern = CREATE_GLOB
    if component:
        pattern = f"projects/ores.sql/create/{component}/**/*_create.sql"
    tables = {}
    for path in sorted(glob.glob(pattern, recursive=True)):
        text = pathlib.Path(path).read_text()
        m = TABLE_RE.search(text)
        if not m:
            continue
        table, body = m.group(1), m.group(2)
        cols = {}
        for name, rest in COLUMN_RE.findall(body):
            cols[name] = re.sub(r",\s*$", "", rest)
        if cols:
            tables[table] = cols
    return tables


def live_columns(sql):
    r = sql("select table_name, column_name from information_schema.columns "
            "where table_schema = 'public';")
    live = {}
    for line in r.stdout.strip().splitlines():
        if not line:
            continue
        table, _, column = line.partition("\x1f")
        live.setdefault(table, set()).add(column)
    return live


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--component", help="limit to one component, e.g. trading")
    ap.add_argument("--emit-alters", action="store_true",
                    help="print the ALTER statements that would close the gap")
    args = ap.parse_args()

    tables = declared_columns(args.component)
    if not tables:
        sys.exit(f"No create scripts matched under {CREATE_GLOB}.")
    live = live_columns(make_sql(read_env()))

    missing = []
    for table, cols in sorted(tables.items()):
        if table not in live:
            sys.exit(f"Table {table} is declared but absent; "
                     "the create aggregate has not run against this database.")
        for column, definition in cols.items():
            if column not in live[table]:
                missing.append((table, column, definition))

    if args.emit_alters:
        for table, column, definition in missing:
            print(f'alter table "{table}" add column if not exists '
                  f'"{column}" {definition};')
        return 0

    if not missing:
        print(f"{len(tables)} tables, every declared column present.")
        return 0

    print(f"{len(missing)} declared columns are absent:")
    for table, column, _ in missing:
        print(f"  {table}.{column}")
    print("\nClose the gap with --emit-alters, or restore the database.")
    return 1


if __name__ == "__main__":
    sys.exit(main())
