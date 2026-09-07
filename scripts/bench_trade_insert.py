#!/usr/bin/env python3
"""Measure trade insert throughput, with and without the per-row notify trigger.

The generated trade schema attaches a notify trigger declared "for each row",
whose function calls pg_notify once per inserted row. This measures what that
costs at several volumes, so the trade population design is settled by a number
rather than by an argument.

Reaches postgres through psql, the way the rest of the project does. Reads
connection settings from .env. Creates and drops its own table, so it touches
nothing the system owns.

  ./scripts/bench_trade_insert.py
  ./scripts/bench_trade_insert.py --volumes 1000,10000,100000,1000000
"""
import argparse
import os
import pathlib
import statistics
import subprocess
import sys
import time

TABLE = "bench_trade_insert_tmp"

SETUP = f"""
drop table if exists {TABLE} cascade;
drop function if exists {TABLE}_notify_fn cascade;
create table {TABLE} (
    instrument_id text not null,
    tenant_id text not null,
    trade_date date not null,
    maturity_date date not null,
    notional numeric not null,
    currency text not null,
    counterparty text not null,
    portfolio_code text not null,
    valid_from timestamptz not null default now(),
    valid_to timestamptz
);
create function {TABLE}_notify_fn() returns trigger as $$
begin
    perform pg_notify('{TABLE}', row_to_json(NEW)::text);
    return NEW;
end;
$$ language plpgsql;
"""

TEARDOWN = (f"drop table if exists {TABLE} cascade; "
            f"drop function if exists {TABLE}_notify_fn cascade;")

TRIGGER_ON = (f"drop trigger if exists {TABLE}_trg on {TABLE}; "
              f"create trigger {TABLE}_trg after insert on {TABLE} "
              f"for each row execute function {TABLE}_notify_fn();")
TRIGGER_OFF = f"drop trigger if exists {TABLE}_trg on {TABLE};"


def read_env(path=".env"):
    settings = {}
    p = pathlib.Path(path)
    if not p.exists():
        sys.exit(f"{path} not found; run from the worktree root.")
    for line in p.read_text().splitlines():
        line = line.strip()
        if line and not line.startswith("#") and "=" in line:
            k, _, v = line.partition("=")
            settings[k.strip()] = v.strip().strip('"').strip("'")
    return settings


def psql_argv(env):
    db = env.get("ORES_TEST_DB_DATABASE") or env.get("ORES_DB_NAME")
    if not db:
        sys.exit("No database in .env (ORES_TEST_DB_DATABASE).")
    return ["psql", "-At", "-q", "-v", "ON_ERROR_STOP=1",
            "-h", env.get("PGHOST", "localhost"),
            "-p", env.get("PGPORT", "5432"),
            "-U", env.get("PGUSER", "postgres"), "-d", db]


def run_psql(argv, child_env, sql=None, stdin_text=None):
    args = argv + (["-c", sql] if sql else [])
    r = subprocess.run(args, input=stdin_text, capture_output=True,
                       text=True, env=child_env)
    if r.returncode != 0:
        sys.exit(f"psql failed:\n{r.stderr.strip()}")
    return r


def rows_copy(n):
    cols = "\t".join
    return "".join(
        cols([f"BENCH-{i}", "acme", "2026-01-02", "2031-01-02",
              str(1_000_000 + i), "GBP", f"CPTY-{i % 200}",
              f"acme_group.p{i % 36}"]) + "\n"
        for i in range(n))


def rows_insert(n):
    values = ",".join(
        f"('BENCH-{i}','acme','2026-01-02','2031-01-02',{1_000_000 + i},"
        f"'GBP','CPTY-{i % 200}','acme_group.p{i % 36}')"
        for i in range(n))
    return (f"insert into {TABLE} (instrument_id, tenant_id, trade_date, "
            f"maturity_date, notional, currency, counterparty, portfolio_code) "
            f"values {values};")


def time_load(argv, child_env, n, method):
    run_psql(argv, child_env, sql=f"truncate {TABLE};")
    if method == "copy":
        payload = rows_copy(n)
        stmt = (f"copy {TABLE} (instrument_id, tenant_id, trade_date, "
                f"maturity_date, notional, currency, counterparty, "
                f"portfolio_code) from stdin;\n")
        start = time.perf_counter()
        run_psql(argv, child_env, stdin_text=stmt + payload + "\\.\n")
    else:
        payload = rows_insert(n)
        start = time.perf_counter()
        run_psql(argv, child_env, stdin_text=payload)
    elapsed = time.perf_counter() - start
    got = run_psql(argv, child_env,
                   sql=f"select count(*) from {TABLE};").stdout.strip()
    if got.split("\n")[-1] != str(n):
        sys.exit(f"{method} loaded {got} rows, expected {n}")
    return elapsed


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--volumes", default="1000,10000,100000")
    ap.add_argument("--repeats", type=int, default=3)
    args = ap.parse_args()
    volumes = [int(v) for v in args.volumes.split(",")]

    env = read_env()
    argv = psql_argv(env)
    child_env = os.environ.copy()
    if env.get("PGPASSWORD"):
        child_env["PGPASSWORD"] = env["PGPASSWORD"]

    run_psql(argv, child_env, sql=SETUP)
    try:
        print(f"{'volume':>9} {'method':>8} {'trigger':>8} "
              f"{'seconds':>9} {'rows/sec':>10}")
        for n in volumes:
            for trigger in (True, False):
                run_psql(argv, child_env,
                         sql=TRIGGER_ON if trigger else TRIGGER_OFF)
                for method in ("copy", "insert"):
                    times = [time_load(argv, child_env, n, method)
                             for _ in range(args.repeats)]
                    t = statistics.median(times)
                    print(f"{n:>9} {method:>8} {str(trigger):>8} "
                          f"{t:>9.3f} {n / t:>10.0f}")
    finally:
        run_psql(argv, child_env, sql=TEARDOWN)


if __name__ == "__main__":
    main()
