#!/usr/bin/env python3
"""Load a fully-populated trade population into the real trading schema, and time it.

Every column of every ores_trading table is filled, including the nullable ones
and the JSON schedule columns, so the timings reflect what a real population
costs rather than what a sparse one costs.

The script introspects the deployed schema rather than carrying a copy of it, so
it stays correct as the model changes, and reports anything it cannot satisfy as
a finding instead of failing silently. Two outputs: a per-table cost table that
shows where the time goes, and a findings list that shows where the model
resists being populated.

The bond family (pilot task D7943D7E) loads to its model premise rather than to
uniform counts. Its ten trade-type codes cycle over the bond instrument rows, so
each block of ten consecutive rows trades one ISIN; the issue row of that ISIN,
the issue-keyed child rows and the per-product fact rows load one per block.
The family tables therefore load at rows / 10; every other table loads at rows.

Reaches postgres through psql. Deletes the rows it inserted unless --keep.

  ./scripts/load_trade_population.py --rows 1000
  ./scripts/load_trade_population.py --rows 20000 --keep
"""
import argparse
import os
import pathlib
import re
import subprocess
import sys
import time
import uuid

MARKER = "bench-load"
NS = uuid.UUID("00000000-0000-0000-0000-00000000ffff")

# Tenancy and workspace are enforced by trigger, not by foreign key, so these
# must be real. Both are resolved from the database rather than hardcoded.
TENANT_SQL = ("select id from ores_iam_tenants_tbl t "
              "join ores_iam_tenant_statuses_tbl s on s.code = t.status "
              "where t.status = 'active' order by t.id limit 1;")
# Trades need a book, and a book belongs to a tenant, so the tenant to load
# into is whichever active one has reference data. Any other choice loads the
# instrument tables and then fails on trades.
TENANT_FALLBACK_SQL = (
    "select t.id from ores_iam_tenants_tbl t "
    "where t.status::text = 'active' "
    "order by (select count(*) from ores_refdata_books_tbl b "
    "          where b.tenant_id = t.id) desc, t.id limit 1;")
WORKSPACE_SQL = "select ores_utility_live_workspace_id_fn();"
PARTY_SQL = "select id from ores_iam_parties_tbl order by id limit 1;"
COUNTERPARTY_SQL = ("select id from ores_refdata_counterparties_tbl "
                    "where tenant_id = '{tenant}' "
                    "and valid_to = ores_utility_infinity_timestamp_fn() "
                    "order by id limit 1;")

# Optional self-references cannot be satisfied on a first insert, because the
# row they would point at does not exist yet. They are left empty rather than
# filled with an id the validation will reject.
NULLABLE_SELF_REFS = {"successor_trade_id"}

STATUS_SQL = ("select s.id from ores_dq_fsm_states_tbl s "
              "where s.valid_to = ores_utility_infinity_timestamp_fn() "
              "order by s.is_initial desc, s.id limit 1;")
REASON_SQL = ("select code from ores_dq_change_reasons_tbl "
              "where code = 'system.test' union all "
              "select code from ores_dq_change_reasons_tbl limit 1;")

# The insert triggers read a session variable, so every statement that loads a
# row must carry it. COPY runs in the same session as the SET that precedes it.
SESSION_PREAMBLE = "set app.current_party_id = '{party}';\n"

# A trade's portfolio_id must equal its book's parent_portfolio_id; the trigger
# checks the two agree, so both come from the same row.
BOOK_SQL = ("select id, parent_portfolio_id from ores_refdata_books_tbl "
            "where tenant_id = '{tenant}' "
            "and valid_to = ores_utility_infinity_timestamp_fn() "
            "and parent_portfolio_id is not null order by id limit 1;")

# The insert triggers stamp modified_by and performed_by with a validated value,
# so a text value of the loader's own making is rejected. Any account username
# passes; service accounts keep the bench rows from impersonating a user.
ACCOUNT_SQL = ("select username from ores_iam_accounts_tbl "
               "where valid_to = ores_utility_infinity_timestamp_fn() "
               "order by (account_type = 'service') desc, username limit 1;")

# The legacy bond instrument table keeps its name through the reshape, so the
# family entries below stay valid when the reshaped table replaces it.
BOND_INSTRUMENTS = "ores_trading_bond_instruments_tbl"
BOND_ISSUES = "ores_trading_bond_issues_tbl"

# The ten bond trade-type codes, in declaration order. One trade of each code
# loads per ISIN, so the j-th instrument row of a code sits at
# j * len(codes) + code_index, and that row belongs to issue block j. The
# facts and children load one row per block, aligned to the same index.
TRADE_TYPE_CODES = ("Bond", "ForwardBond", "BondFuture", "BondOption",
                    "BondRepo", "BondTRS", "BondPosition", "CallableBond",
                    "ConvertibleBond", "Ascot")

# The five codes whose product carries structure beyond the instrument row.
# Each fact table extends the instrument rows of its own code.
FACT_TABLES = {
    "BondOption": "ores_trading_bond_options_tbl",
    "BondFuture": "ores_trading_bond_futures_tbl",
    "BondRepo": "ores_trading_bond_repos_tbl",
    "BondTRS": "ores_trading_bond_trs_tbl",
    "Ascot": "ores_trading_ascots_tbl",
}

FACT_CODE = {t: c for c, t in FACT_TABLES.items()}
CHILD_TABLES = ("ores_trading_bond_issue_call_dates_tbl",
                "ores_trading_bond_issue_conversion_targets_tbl")

# Soft foreign keys, enforced in PL/pgSQL. A referencing column must carry an id
# the referenced table already holds, so the referenced table loads first. Row
# i of the referencing table couples to row i of the referenced one.
REFERENCES = {
    ("ores_trading_composite_legs_tbl", "instrument_id"):
        ("ores_trading_composite_instruments_tbl", "id"),
    ("ores_trading_party_roles_tbl", "trade_id"):
        ("ores_trading_trades_tbl", "id"),
    ("ores_trading_trade_identifiers_tbl", "trade_id"):
        ("ores_trading_trades_tbl", "id"),
    ("ores_trading_bond_issue_call_dates_tbl", "issue_id"):
        ("ores_trading_bond_issues_tbl", "issue_id"),
    ("ores_trading_bond_issue_conversion_targets_tbl", "issue_id"):
        ("ores_trading_bond_issues_tbl", "issue_id"),
}

INTROSPECT_COLUMNS = """
select c.table_name, c.column_name, c.data_type, c.is_nullable, c.udt_name
from information_schema.columns c
join information_schema.tables t
  on t.table_name = c.table_name and t.table_schema = c.table_schema
where c.table_name like 'ores_trading%' and t.table_type = 'BASE TABLE'
order by c.table_name, c.ordinal_position;
"""

INTROSPECT_CHECKS = """
select conrelid::regclass::text, pg_get_constraintdef(oid)
from pg_constraint
where contype = 'c' and conrelid::regclass::text like 'ores_trading%';
"""

INTROSPECT_ENUMS = """
select t.typname, e.enumlabel
from pg_enum e join pg_type t on t.oid = e.enumtypid
order by t.typname, e.enumsortorder;
"""

ALLOWED_RE = re.compile(r"\(?(\w+)\s*=\s*ANY\s*\(ARRAY\[(.*?)\]\)", re.S)
# Several enumerations are enforced in PL/pgSQL rather than by a check
# constraint or a reference table. The trigger names the valid set when it
# rejects a row, so the loader learns them from the error and retries.
LEARN_RE = re.compile(r"Invalid \w+: (\S+?)\.\s*Must be one of:\s*([^|\n]+)")
LITERAL_RE = re.compile(r"'([^']*)'")


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

    def sql(stmt=None, stdin=None, check=True):
        r = subprocess.run(argv + (["-c", stmt] if stmt else []), input=stdin,
                           capture_output=True, text=True, env=ce)
        if check and r.returncode != 0:
            raise RuntimeError(r.stderr.strip())
        return r
    return sql


def rows_of(result):
    return [line.split("\x1f") for line in result.stdout.strip().splitlines()
            if line]


def schedule_json(n=40):
    dates = ",".join(f'"{2027 + i // 4}-{3 * (i % 4) + 1:02d}-15"'
                     for i in range(n))
    return f'{{"dates":[{dates}],"convention":"Following"}}'


class Generator:
    """Produces a value for one column, given its type and any check constraint."""

    def __init__(self, allowed, bounds, enums, tenant, workspace, party, reason,
                 book, portfolio, account=None, counterparty=None, status=None,
                 required_null=None):
        self.required_null = required_null or {}
        self.counterparty = counterparty
        self.status = status
        self.book = book
        self.portfolio = portfolio
        self.allowed = allowed
        self.bounds = bounds
        self.enums = enums
        self.tenant = tenant
        self.workspace = workspace
        self.party = party
        self.reason = reason
        self.account = account

    def value(self, table, col, dtype, udt, i):
        if col == "tenant_id":
            return self.tenant
        if col == "workspace_id":
            return self.workspace
        if col == "change_reason_code":
            return self.reason
        if col in ("modified_by", "performed_by") and self.account:
            return self.account
        if col in NULLABLE_SELF_REFS:
            return "\\N"
        if col in self.required_null.get(table, ()):
            return "\\N"
        if col == "counterparty_id" and self.counterparty:
            return self.counterparty
        if col == "status_id" and self.status:
            return self.status
        if col == "party_id":
            return self.party
        if col == "book_id" and self.book:
            return self.book
        if col == "portfolio_id" and self.portfolio:
            return self.portfolio
        if table == BOND_INSTRUMENTS:
            if col == "trade_type_code":
                return TRADE_TYPE_CODES[i % len(TRADE_TYPE_CODES)]
            if col == "issue_id":
                # Row i trades the i // len(codes) issue's ISIN. Inert while the
                # legacy table lacks the column; the reshaped instrument rows
                # carry it, and the seed couples them to the loaded issue.
                return str(uuid.uuid5(NS, f"{BOND_ISSUES}.issue_id."
                                        f"{i // len(TRADE_TYPE_CODES)}"))
        if col == "security_id" and table == BOND_ISSUES:
            return f"XS{i:010d}"
        if col == "instrument_id" and table in FACT_CODE:
            # The fact row of the j-th instrument row of its own product code.
            return str(uuid.uuid5(
                NS, f"{BOND_INSTRUMENTS}.id."
                    f"{i * len(TRADE_TYPE_CODES) + TRADE_TYPE_CODES.index(FACT_CODE[table])}"))
        ref = REFERENCES.get((table, col))
        if ref:
            return str(uuid.uuid5(NS, f"{ref[0]}.{ref[1]}.{i}"))
        key = (table, col)
        if key in self.allowed:
            return self.allowed[key][i % len(self.allowed[key])]
        if dtype == "USER-DEFINED":
            labels = self.enums.get(udt)
            if not labels:
                raise KeyError(f"unknown enum {udt}")
            return labels[i % len(labels)]
        if dtype == "uuid":
            return str(uuid.uuid5(NS, f"{table}.{col}.{i}"))
        if dtype == "integer":
            return "1"
        if dtype == "boolean":
            return "true"
        if dtype == "numeric":
            cap = self.bounds.get((table, col))
            if cap is not None:
                return f"{min(cap, 1) * 0.5:.4f}" if cap <= 1 else f"{cap / 2:.2f}"
            return f"{1000000 + i}.50"
        if dtype == "double precision":
            return "0.0125"
        if dtype == "date":
            # satisfy the common maturity > start ordering
            return "2031-01-02" if _is_late(col) else "2026-01-02"
        if dtype.startswith("timestamp"):
            return "2027-01-01 00:00:00+00" if col == "valid_to" \
                else "2026-01-01 00:00:00+00"
        if dtype == "text":
            if col.endswith("_json"):
                return schedule_json()
            if col == "change_commentary":
                return MARKER
            return f"{col}-{i}"
        raise KeyError(f"unhandled type {dtype}")


def _learn(stderr, table, gen):
    """Register an enumeration the trigger named while rejecting the row."""
    m = LEARN_RE.search(stderr)
    if not m:
        return None
    bad, values = m.group(1), [v.strip() for v in m.group(2).split(",")]
    col = bad.rsplit("-", 1)[0]
    if (table, col) in gen.allowed or not values:
        return None
    gen.allowed[(table, col)] = values
    return f"{col} = {{{', '.join(values[:6])}{' ...' if len(values) > 6 else ''}}}"


def _first_error(stderr):
    """The first real ERROR and its DETAIL, ignoring the collation warning."""
    lines = [l.strip() for l in stderr.splitlines()]
    out = []
    for i, l in enumerate(lines):
        if l.startswith("ERROR:"):
            out.append(l)
            for nxt in lines[i + 1:i + 3]:
                if nxt.startswith(("DETAIL:", "CONTEXT:")):
                    out.append(nxt)
            break
    return " | ".join(out)[:220] if out else "no ERROR line in psql output"


def _is_late(col):
    return any(k in col for k in
               ("maturity", "termination", "expiry", "end_", "_to", "valid_to"))


RANGE_RE = re.compile(r"\((\w+)\s*(<=|<)\s*\(?([0-9.]+)")
EQ_RE = re.compile(r"\((\w+)\s*=\s*'([^']+)'::text\)")
NULL_RE = re.compile(r"\((\w+) IS NULL\)")


def parse_allowed(check_rows):
    """Value sets, upper bounds and required nulls the check constraints impose."""
    allowed, bounds, required_null = {}, {}, {}
    for table, defn in check_rows:
        for m in ALLOWED_RE.finditer(defn):
            col, body = m.group(1), m.group(2)
            vals = LITERAL_RE.findall(body)
            if vals:
                allowed.setdefault((table, col), vals)
        # A table holding two products discriminates them with an equality, and
        # each branch names both the columns that product uses and the columns
        # belonging to the other product, which must be null. Pick one branch
        # and honour both halves: no row can carry every column.
        branches = [b for b in defn.split(" OR ") if "IS NOT NULL" in b]
        if branches:
            chosen = branches[0]
            for m in EQ_RE.finditer(chosen):
                allowed[(table, m.group(1))] = [m.group(2)]
            for m in NULL_RE.finditer(chosen):
                required_null.setdefault(table, set()).add(m.group(1))
        for m in EQ_RE.finditer(defn):
            col, val = m.group(1), m.group(2)
            allowed.setdefault((table, col), [val])
        for m in RANGE_RE.finditer(defn):
            col, cap = m.group(1), float(m.group(3))
            key = (table, col)
            bounds[key] = min(bounds.get(key, cap), cap)
    return allowed, bounds, required_null


def _cleanup(sql, cols, keep):
    """Remove every row this script has ever written, in this run or an earlier one.

    Sweeps all tables rather than only the ones this run loaded, because an
    interrupted or early-returning run leaves rows the run's own result list
    does not name.
    """
    if keep:
        print("\nrows retained (--keep)")
        return

    # These tables are bitemporal. A DELETE is rewritten by an ON DELETE rule
    # into an UPDATE that closes the row's validity, so it never removes
    # anything. Removing what this script wrote means suspending that rule,
    # which is why the rules are restored in a finally.
    disabled = []
    try:
        for t in cols:
            r = sql(f"select rulename from pg_rules where tablename = '{t}' "
                    f"and rulename like '%delete%';", check=False)
            for rule in r.stdout.split():
                if sql(f"alter table {t} disable rule {rule};",
                       check=False).returncode == 0:
                    disabled.append((t, rule))
            sql(f"delete from {t} where change_commentary = '{MARKER}';",
                check=False)
    finally:
        for t, rule in disabled:
            sql(f"alter table {t} enable rule {rule};", check=False)

    left = sql("select coalesce(sum(n), 0) from (" + " union all ".join(
        f"select count(*) as n from {t} where change_commentary = '{MARKER}'"
        for t in cols) + ") s;", check=False)
    n = left.stdout.strip().splitlines()[-1] if left.returncode == 0 else "?"
    off = sql("select count(*) from pg_class c join pg_rewrite w on "
              "w.ev_class = c.oid where c.relname like 'ores_trading%' "
              "and w.ev_enabled = 'D';", check=False)
    d = off.stdout.strip().splitlines()[-1] if off.returncode == 0 else "?"
    print(f"\ncleanup: {n} marked row(s) remain, {d} rule(s) left disabled "
          f"(both must be 0)")


def _attribute(sql, table, cols, gen, preamble, rows):
    """Time one table with its triggers progressively disabled.

    The loader shows where time goes across tables. This shows what the time is
    spent on within one, which the aggregate cannot.
    """
    spec = cols.get(table)
    if not spec:
        sys.exit(f"unknown table {table}")
    names = [c for c, _, _, _ in spec]
    payload = "\n".join(
        "\t".join(gen.value(table, c, d, u, i).replace("\t", " ")
                  for c, d, _, u in spec)
        for i in range(rows)) + "\n"
    stmt = f"copy {table} ({', '.join(names)}) from stdin;\n"

    trg = [l for l in sql(
        f"select tgname from pg_trigger where tgrelid='{table}'::regclass "
        f"and not tgisinternal order by tgname;").stdout.split() if l]
    # The notify trigger only publishes; the insert trigger populates columns
    # the row needs, so disabling it may make the insert invalid. Remove the
    # publishing one first so its cost is isolated before anything breaks.
    trg.sort(key=lambda n: "notify" not in n)
    print(f"{table}: {len(trg)} trigger(s) — {', '.join(trg)}\n")

    def timed():
        sql(f"delete from {table} where change_reason_code = "
            f"(select '{gen.reason}');", check=False)
        start = time.perf_counter()
        r = sql(stdin=preamble + stmt + payload + "\\.\n", check=False)
        if r.returncode != 0:
            return _first_error(r.stderr)
        return time.perf_counter() - start

    disabled = []
    try:
        label = "all triggers enabled"
        while True:
            t = timed()
            if isinstance(t, float):
                print(f"  {label:<44} {rows / t:>9.0f} rows/sec")
            else:
                print(f"  {label:<44}      cannot load\n     {t}")
            if not trg:
                break
            nxt = trg.pop(0)
            sql(f"alter table {table} disable trigger {nxt};")
            disabled.append(nxt)
            label = f"without {nxt.replace(table + '_', '')}"
    finally:
        for name in disabled:
            sql(f"alter table {table} enable trigger {name};", check=False)
        sql(f"delete from {table} where change_reason_code = "
            f"(select '{gen.reason}');", check=False)
        left = sql(f"select count(*) from pg_trigger where "
                   f"tgrelid='{table}'::regclass and not tgisinternal "
                   f"and tgenabled <> 'O';").stdout.strip().splitlines()[-1]
        print(f"\ntriggers left disabled: {left} (must be 0)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--rows", type=int, default=1000)
    ap.add_argument("--keep", action="store_true",
                    help="leave the loaded rows in place")
    ap.add_argument("--attribute", metavar="TABLE",
                    help="time one table with each trigger disabled in turn, "
                         "to attribute the cost")
    args = ap.parse_args()
    if args.rows % len(TRADE_TYPE_CODES):
        sys.exit(f"--rows must be a multiple of {len(TRADE_TYPE_CODES)}: the "
                 f"family loads one issue, child and fact row per block of "
                 f"{len(TRADE_TYPE_CODES)} instrument rows, so a partial "
                 f"block would reference an issue row that never loads.")

    sql = make_sql(read_env())

    cols = {}
    for t, c, dtype, nullable, udt in rows_of(sql(INTROSPECT_COLUMNS)):
        cols.setdefault(t, []).append((c, dtype, nullable, udt))
    allowed, bounds, required_null = parse_allowed(rows_of(sql(INTROSPECT_CHECKS)))
    enums = {}
    for name, label in rows_of(sql(INTROSPECT_ENUMS)):
        enums.setdefault(name, []).append(label)
    r = sql(TENANT_FALLBACK_SQL, check=False)
    tenant = r.stdout.strip().splitlines()[-1] if r.returncode == 0 and \
        r.stdout.strip() else None
    if not tenant:
        sys.exit("No active tenant found; the schema enforces tenancy by trigger.")
    workspace = sql(WORKSPACE_SQL).stdout.strip().splitlines()[-1]
    r = sql(PARTY_SQL, check=False)
    party = r.stdout.strip().splitlines()[-1] if r.returncode == 0 and \
        r.stdout.strip() else tenant
    reason = sql(REASON_SQL).stdout.strip().splitlines()[-1]
    r = sql(BOOK_SQL.format(tenant=tenant), check=False)
    book_row = r.stdout.strip().splitlines()[-1].split("\x1f") \
        if r.returncode == 0 and r.stdout.strip() else []
    book = book_row[0] if book_row else None
    portfolio = book_row[1] if len(book_row) > 1 else None
    r = sql(ACCOUNT_SQL, check=False)
    account = r.stdout.strip().splitlines()[-1] if r.returncode == 0 and \
        r.stdout.strip() else None
    r = sql(COUNTERPARTY_SQL.format(tenant=tenant), check=False)
    counterparty = r.stdout.strip().splitlines()[-1] if r.returncode == 0 and \
        r.stdout.strip() else None
    r = sql(STATUS_SQL, check=False)
    status = r.stdout.strip().splitlines()[-1] if r.returncode == 0 and \
        r.stdout.strip() else None
    gen = Generator(allowed, bounds, enums, tenant, workspace, party, reason,
                    book, portfolio, account, counterparty, status,
                    required_null)
    preamble = SESSION_PREAMBLE.format(party=party)
    print(f"tenant {tenant}\nworkspace {workspace}\nparty {party}\n"
          f"change reason {reason}\nbook {book}  portfolio {portfolio}\n"
          f"account {account}")

    family = {BOND_ISSUES, *CHILD_TABLES, *FACT_TABLES.values()}
    counts = {t: args.rows // len(TRADE_TYPE_CODES) if t in family else args.rows
              for t in cols}
    print(f"{len(cols)} tables, {sum(len(v) for v in cols.values())} columns, "
          f"{args.rows} rows per table"
          f"{', family at ' + str(counts[next(iter(family))]) if family <= set(cols) else ''}\n")

    results, findings, discovered = [], [], []
    if args.attribute:
        try:
            _attribute(sql, args.attribute, cols, gen, preamble, args.rows)
        finally:
            _cleanup(sql, cols, args.keep)
        return

    referenced = [t for t, _ in REFERENCES.values()]
    order = sorted(cols, key=lambda t: (t not in referenced, t))
    for table in order:
        spec = cols[table]
        names = [c for c, _, _, _ in spec]
        count = counts[table]
        try:
            body = []
            for i in range(count):
                body.append("\t".join(
                    gen.value(table, c, d, u, i).replace("\t", " ")
                    for c, d, _, u in spec))
            payload = "\n".join(body) + "\n"
        except KeyError as e:
            findings.append((table, f"cannot generate: {e}"))
            continue

        stmt = f"copy {table} ({', '.join(names)}) from stdin;\n"
        for attempt in range(6):
            start = time.perf_counter()
            r = sql(stdin=preamble + stmt + payload + "\\.\n", check=False)
            elapsed = time.perf_counter() - start
            if r.returncode == 0:
                results.append((table, count, len(spec), elapsed))
                break
            learned = _learn(r.stderr, table, gen)
            if not learned:
                findings.append((table, _first_error(r.stderr)))
                break
            discovered.append((table, learned))
            try:
                payload = "\n".join(
                    "\t".join(gen.value(table, c, d, u, i).replace("\t", " ")
                               for c, d, _, u in spec)
                    for i in range(count)) + "\n"
            except KeyError as e:
                findings.append((table, f"cannot generate: {e}"))
                break
        else:
            findings.append((table, "still rejected after learning 6 sets"))

    results.sort(key=lambda r: -r[3])
    total = sum(e for _, _, _, e in results)
    print(f"{'table':<52} {'rows':>6} {'cols':>5} {'seconds':>9} "
          f"{'rows/sec':>10} {'%':>6}")
    for t, c, n, e in results:
        print(f"{t:<52} {c:>6} {n:>5} {e:>9.3f} {c / e:>10.0f} "
              f"{100 * e / total:>5.1f}%")
    print(f"\n{len(results)} tables loaded in {total:.2f}s "
          f"({sum(c for _, c, _, _ in results):,} rows)")

    if discovered:
        print(f"\n{len(discovered)} enumeration(s) learned from trigger errors, "
              f"enforced in PL/pgSQL rather than by constraint or reference table:")
        for t, d in discovered:
            print(f"  {t}\n     {d}")

    if findings:
        print(f"\n{len(findings)} table(s) could not be populated:")
        for t, why in findings:
            print(f"  {t}\n     {why}")

    _cleanup(sql, cols, args.keep)


if __name__ == "__main__":
    main()
