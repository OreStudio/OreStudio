#!/usr/bin/env python3
"""Measure trade insert throughput, with and without the per-row notify trigger.

The trade tables carry a notify trigger declared "for each row", which calls
pg_notify once per inserted row. This measures what that costs at several
volumes, so the trade population design is decided by a number rather than by
an argument.

Reads connection settings from .env. Creates and drops its own table, so it
touches nothing the system owns.

  ./scripts/bench_trade_insert.py --volumes 1000,10000,100000
"""
import argparse
import os
import pathlib
import statistics
import time

try:
    import psycopg
except ImportError:
    raise SystemExit("psycopg is required: pip install 'psycopg[binary]'")

TABLE = "bench_trade_insert_tmp"

DDL = f"""
drop table if exists {TABLE} cascade;
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
create or replace function {TABLE}_notify_fn() returns trigger as $$
begin
    perform pg_notify('{TABLE}', row_to_json(NEW)::text);
    return NEW;
end;
$$ language plpgsql;
"""

TRIGGER_ON = f"""
create trigger {TABLE}_notify_trg after insert on {TABLE}
for each row execute function {TABLE}_notify_fn();
"""

TRIGGER_OFF = f"drop trigger if exists {TABLE}_notify_trg on {TABLE};"


def env(path=".env"):
    settings = {}
    p = pathlib.Path(path)
    if p.exists():
        for line in p.read_text().splitlines():
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                k, _, v = line.partition("=")
                settings[k.strip()] = v.strip().strip('"').strip("'")
    return settings


def conninfo():
    s = env()
    return (
        f"host={s.get('ORES_DB_HOST', 'localhost')} "
        f"port={s.get('ORES_DB_PORT', '5432')} "
        f"dbname={s.get('ORES_DB_NAME', 'ores')} "
        f"user={s.get('ORES_DB_USER', 'ores')} "
        f"password={s.get('ORES_DB_PASSWORD', '')}"
    )


def rows(n):
    for i in range(n):
        yield (
            f"BENCH-{i}", "acme", "2026-01-02", "2031-01-02",
            1_000_000 + i, "GBP", f"CPTY-{i % 200}", f"acme_group.p{i % 36}",
        )


def time_insert(cur, n, method):
    cur.execute(f"truncate {TABLE};")
    start = time.perf_counter()
    if method == "copy":
        with cur.copy(f"copy {TABLE} (instrument_id, tenant_id, trade_date, "
                      f"maturity_date, notional, currency, counterparty, "
                      f"portfolio_code) from stdin") as cp:
            for r in rows(n):
                cp.write_row(r)
    else:
        cur.executemany(
            f"insert into {TABLE} (instrument_id, tenant_id, trade_date, "
            f"maturity_date, notional, currency, counterparty, portfolio_code) "
            f"values (%s,%s,%s,%s,%s,%s,%s,%s)",
            list(rows(n)),
        )
    return time.perf_counter() - start


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--volumes", default="1000,10000,100000")
    ap.add_argument("--repeats", type=int, default=3)
    args = ap.parse_args()
    volumes = [int(v) for v in args.volumes.split(",")]

    with psycopg.connect(conninfo(), autocommit=True) as conn:
        with conn.cursor() as cur:
            cur.execute(DDL)
            print(f"{'volume':>9} {'method':>10} {'trigger':>8} "
                  f"{'seconds':>9} {'rows/sec':>10}")
            for n in volumes:
                for trigger in (True, False):
                    cur.execute(TRIGGER_ON if trigger else TRIGGER_OFF)
                    for method in ("copy", "insert"):
                        times = [time_insert(cur, n, method)
                                 for _ in range(args.repeats)]
                        t = statistics.median(times)
                        print(f"{n:>9} {method:>10} {str(trigger):>8} "
                              f"{t:>9.3f} {n / t:>10.0f}")
            cur.execute(f"drop table if exists {TABLE} cascade;")
            cur.execute(f"drop function if exists {TABLE}_notify_fn cascade;")


if __name__ == "__main__":
    main()
