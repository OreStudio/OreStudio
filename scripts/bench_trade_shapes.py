#!/usr/bin/env python3
"""Measure trade insert cost by trade shape, not by row count.

A trade is not one row. Every trade writes a trade row, an instrument row, and
some number of identifier and party-role rows; a swap writes two leg rows on top.
A million forwards and a million swaps are therefore different amounts of work,
across a different number of tables, each carrying its own per-row notify
trigger. This measures both shapes end to end.

Field counts and fan-out mirror the entities in ores.trading.core/repository.
Reaches postgres through psql. Creates and drops its own tables.

  ./scripts/bench_trade_shapes.py
  ./scripts/bench_trade_shapes.py --volumes 100000,1000000
"""
import argparse
import os
import pathlib
import statistics
import subprocess
import sys
import time

PREFIX = "bench_shape"


def table(name, fields):
    cols = ", ".join(f"f{i} text" for i in range(fields - 2))
    return (f"{PREFIX}_{name}", fields,
            f"create table {PREFIX}_{name} (id text not null, "
            f"trade_id text not null, {cols});")


# name, fields per row, rows per trade, bytes in a json blob column
TRADE = ("trade", 24, 1, 0)
IDENT = ("trade_identifier", 12, 2, 0)
PARTY = ("trade_party_role", 9, 2, 0)
LEGS = ("swap_leg", 19, 2, 0)

# A callable swap stores its exercise schedule in call_dates_json rather than in
# a child table. Forty quarterly call dates as ISO dates in a JSON array.
CALL_DATES_BYTES = 40 * 14

SHAPES = {
    "forward": [TRADE, ("fx_forward_instrument", 15, 1, 0), IDENT, PARTY],
    "swap": [TRADE, ("vanilla_swap_instrument", 15, 1, 0), LEGS, IDENT, PARTY],
    "swaption": [TRADE, ("swaption_instrument", 17, 1, 0), LEGS, IDENT, PARTY],
    "callable_swap": [TRADE,
                      ("callable_swap_instrument", 15, 1, CALL_DATES_BYTES),
                      LEGS, IDENT, PARTY],
}


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


def make_sql(env):
    db = env.get("ORES_TEST_DB_DATABASE")
    if not db:
        sys.exit("No ORES_TEST_DB_DATABASE in .env.")
    argv = ["psql", "-At", "-q", "-v", "ON_ERROR_STOP=1",
            "-h", env.get("PGHOST", "localhost"),
            "-p", env.get("PGPORT", "5432"),
            "-U", env.get("PGUSER", "postgres"), "-d", db]
    ce = os.environ.copy()
    if env.get("PGPASSWORD"):
        ce["PGPASSWORD"] = env["PGPASSWORD"]

    def sql(stmt=None, stdin=None):
        r = subprocess.run(argv + (["-c", stmt] if stmt else []), input=stdin,
                           capture_output=True, text=True, env=ce)
        if r.returncode != 0:
            sys.exit(f"psql failed:\n{r.stderr.strip()}")
        return r.stdout.strip()
    return sql


def create(sql, shape, trigger):
    for name, fields, _, blob in shape:
        t = f"{PREFIX}_{name}"
        sql(f"drop table if exists {t} cascade;")
        cols = ", ".join(f"f{i} text" for i in range(fields - 2))
        blob_col = ", blob text" if blob else ""
        sql(f"create table {t} (id text not null, trade_id text not null, "
            f"{cols}{blob_col});")
        sql(f"create or replace function {t}_fn() returns trigger as $$ begin "
            f"perform pg_notify('{t}', row_to_json(NEW)::text); return NEW; "
            f"end; $$ language plpgsql;")
        if trigger:
            sql(f"create trigger {t}_trg after insert on {t} "
                f"for each row execute function {t}_fn();")


def drop(sql, shape):
    for name, _, _, _ in shape:
        sql(f"drop table if exists {PREFIX}_{name} cascade;")
        sql(f"drop function if exists {PREFIX}_{name}_fn cascade;")


def payload(name, fields, per_trade, n, blob):
    filler = ("2027-03-15," * (blob // 11))[:blob] if blob else None
    out = []
    for i in range(n):
        for k in range(per_trade):
            cells = [f"{name}-{i}-{k}", f"T-{i}"]
            cells += [f"v{j}" for j in range(fields - 2)]
            if blob:
                cells.append(filler)
            out.append("\t".join(cells))
    return "\n".join(out) + "\n"


def run(sql, shape, n, method):
    loads = []
    for name, fields, per_trade, blob in shape:
        t = f"{PREFIX}_{name}"
        cols = ", ".join(["id", "trade_id"] +
                         [f"f{j}" for j in range(fields - 2)] +
                         (["blob"] if blob else []))
        filler = ("2027-03-15," * (blob // 11))[:blob] if blob else None
        if method == "copy":
            body = f"copy {t} ({cols}) from stdin;\n" + \
                payload(name, fields, per_trade, n, blob) + "\\.\n"
        else:
            rows = []
            for i in range(n):
                for k in range(per_trade):
                    vals = [f"'{name}-{i}-{k}'", f"'T-{i}'"]
                    vals += [f"'v{j}'" for j in range(fields - 2)]
                    if blob:
                        vals.append(f"'{filler}'")
                    rows.append("(" + ",".join(vals) + ")")
            body = f"insert into {t} ({cols}) values " + ",".join(rows) + ";"
        loads.append((t, body, n * per_trade))
    start = time.perf_counter()
    for _, body, _ in loads:
        sql(stdin=body)
    elapsed = time.perf_counter() - start
    for t, _, expected in loads:
        got = sql(f"select count(*) from {t};")
        if got != str(expected):
            sys.exit(f"{t}: loaded {got}, expected {expected}")
    return elapsed, sum(e for _, _, e in loads)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--volumes", default="10000,100000")
    ap.add_argument("--repeats", type=int, default=3)
    ap.add_argument("--shapes", default="forward,swap,swaption,callable_swap")
    args = ap.parse_args()
    volumes = [int(v) for v in args.volumes.split(",")]
    sql = make_sql(read_env())

    print(f"{'shape':>8} {'trades':>9} {'rows':>10} {'method':>7} "
          f"{'trigger':>8} {'seconds':>9} {'trades/sec':>11}")
    wanted = args.shapes.split(",")
    for shape_name, shape in SHAPES.items():
        if shape_name not in wanted:
            continue
        for trigger in (True, False):
            create(sql, shape, trigger)
            try:
                for n in volumes:
                    for method in ("copy", "insert"):
                        times, rows = [], 0
                        for _ in range(args.repeats):
                            for name, _, _, _ in shape:
                                sql(f"truncate {PREFIX}_{name};")
                            t, rows = run(sql, shape, n, method)
                            times.append(t)
                        med = statistics.median(times)
                        print(f"{shape_name:>8} {n:>9} {rows:>10} {method:>7} "
                              f"{str(trigger):>8} {med:>9.3f} {n / med:>11.0f}")
            finally:
                drop(sql, shape)


if __name__ == "__main__":
    main()
