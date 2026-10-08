#!/usr/bin/env python3
"""Throwaway probe: prove the trade structure tables behave as modelled.

Drives psql from Python because no Postgres driver is installed for this
interpreter. Every case runs inside one transaction that is rolled back, so the
database is left exactly as it was found.

Each case is a plpgsql block that runs a statement and records the SQLSTATE it
got. Successes and refusals are both recorded, so a case that unexpectedly
succeeds is reported as a failure rather than passing silently.

Run::

    python3 build/probe_structures.py
"""
import os
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]


def load_env() -> dict[str, str]:
    env: dict[str, str] = {}
    for line in (REPO_ROOT / ".env").read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        env[key.strip()] = value.strip().strip('"').strip("'")
    return env


ENV = load_env()

# The test DDL role, not the owner: the owner carries BYPASSRLS, and a probe
# that bypasses the policies proves nothing about them.
DSN = [
    "psql",
    "-h", ENV["ORES_TEST_DB_HOST"],
    "-U", ENV["ORES_TEST_DB_DDL_USER"],
    "-d", ENV["ORES_TEST_DB_DATABASE"],
    "-v", "ON_ERROR_STOP=1",
    "-At",
    "-f", "-",
]

FIXTURE = """
insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_counterparty_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000cf104'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'Structure Probe Counterparty', 'PRB-CP', 'Corporate',
    null, 'WRLD', 'Active', current_user, current_user,
    'system.test', 'structure probe'),

    ('00000000-0000-0000-0000-0000000cf204'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'Structure Probe Parent Counterparty', 'PRB-CP2', 'Corporate',
    null, 'WRLD', 'Active', current_user, current_user,
    'system.test', 'structure probe');

create temp table probe_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select id from ores_refdata_counterparties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'PRB-CP'
          and valid_to = ores_utility_infinity_timestamp_fn()) as counterparty_id;
"""

# The harness: three helpers, recording into one temp table.
HARNESS = """
create sequence probe_seq;

create temp table probe_results (
    n        integer,
    kind     text,
    name     text,
    expected text,
    got      text,
    ok       boolean
) on commit drop;

create or replace function pg_temp.record(p_kind text, p_name text,
    p_expected text, p_got text)
returns void as $$
    insert into probe_results
    select nextval('probe_seq'), p_kind, p_name, p_expected, p_got,
           case when p_expected = '?' then null else p_expected = p_got end;
$$ language sql;

create or replace function pg_temp.run(p_name text, p_expected text, p_sql text)
returns void as $$
declare
    got text;
begin
    begin
        execute p_sql;
        got := '00000';
    exception when others then
        got := sqlstate;
    end;
    perform pg_temp.record('case', p_name, p_expected, got);
end;
$$ language plpgsql;

create or replace function pg_temp.expect(p_name text, p_condition boolean)
returns void as $$
begin
    perform pg_temp.record('read', p_name, 'true', p_condition::text);
end;
$$ language plpgsql;

create or replace function pg_temp.structure(p_id uuid, p_parent uuid default null,
    p_kind text default 'Strategy', p_template text default 'Straddle')
returns text as $$
begin
    insert into ores_trading_structures_tbl (id, tenant_id, party_id, counterparty_id,
        kind, template_code, parent_structure_id)
    select p_id, ores_utility_system_tenant_id_fn(), party_id, counterparty_id,
        p_kind, p_template, p_parent
    from probe_ctx;
    return 'inserted';
end;
$$ language plpgsql;
"""

CASES = r"""
-- =============================================================================
-- What the seeds actually contain
-- =============================================================================

select pg_temp.expect('the four rungs are seeded',
    (select count(*) from ores_trading_structure_kinds_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()) = 4);

select pg_temp.expect('a package confirms trade by trade and the rungs above it as a whole',
    (select confirms_as_whole from ores_trading_structure_kinds_tbl
     where code = 'Package' and valid_to = ores_utility_infinity_timestamp_fn()) = false
    and
    (select count(*) from ores_trading_structure_kinds_tbl
     where confirms_as_whole and valid_to = ores_utility_infinity_timestamp_fn()) = 3);

select pg_temp.expect('the five templates are seeded',
    (select count(*) from ores_trading_structure_templates_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()) = 5);

select pg_temp.expect('a butterfly is a body of one and two wings of two',
    (select count(*) from ores_trading_structure_template_roles_tbl
     where template_code = 'Butterfly' and role = 'body' and min_legs = 1 and max_legs = 1
       and valid_to = ores_utility_infinity_timestamp_fn()) = 1
    and
    (select count(*) from ores_trading_structure_template_roles_tbl
     where template_code = 'Butterfly' and role = 'wing' and min_legs = 2 and max_legs = 2
       and valid_to = ores_utility_infinity_timestamp_fn()) = 1);

-- =============================================================================
-- The template and role constraints
-- =============================================================================

select pg_temp.run('a template naming an unknown kind is refused', '23503',
    $q$insert into ores_trading_structure_templates_tbl
        (code, tenant_id, version, description, kind, modified_by, change_reason_code,
         change_commentary)
       values ('ProbeBad', ores_utility_system_tenant_id_fn(), 0, 'probe', 'Nonsense',
               current_user, 'system.new_record', 'probe')$q$);

select pg_temp.run('a role holding a negative number of legs is refused', '23514',
    $q$insert into ores_trading_structure_template_roles_tbl
        (template_code, role, tenant_id, version, min_legs, max_legs, description,
         modified_by, change_reason_code, change_commentary)
       values ('Straddle', 'probe_neg', ores_utility_system_tenant_id_fn(), 0, -1, 1,
               'probe', current_user, 'system.new_record', 'probe')$q$);

select pg_temp.run('a role whose maximum is below its minimum is refused', '23514',
    $q$insert into ores_trading_structure_template_roles_tbl
        (template_code, role, tenant_id, version, min_legs, max_legs, description,
         modified_by, change_reason_code, change_commentary)
       values ('Straddle', 'probe_order', ores_utility_system_tenant_id_fn(), 0, 2, 1,
               'probe', current_user, 'system.new_record', 'probe')$q$);

select pg_temp.run('a template states each role once', '23505',
    $q$insert into ores_trading_structure_template_roles_tbl
        (template_code, role, tenant_id, version, min_legs, max_legs, description,
         modified_by, change_reason_code, change_commentary)
       values ('Butterfly', 'body', ores_utility_system_tenant_id_fn(), 0, 1, 1,
               'probe', current_user, 'system.new_record', 'probe')$q$);

select pg_temp.run('a role naming an unknown template is refused', '23503',
    $q$insert into ores_trading_structure_template_roles_tbl
        (template_code, role, tenant_id, version, min_legs, max_legs, description,
         modified_by, change_reason_code, change_commentary)
       values ('NoSuchTemplate', 'leg', ores_utility_system_tenant_id_fn(), 0, 1, 1,
               'probe', current_user, 'system.new_record', 'probe')$q$);

-- =============================================================================
-- The structure itself
-- =============================================================================

select pg_temp.run('a deal with no parent is written', '00000',
    $q$select pg_temp.structure('00000000-0000-0000-0000-0000000cb001')$q$);

select pg_temp.run('a deal that is a leg names its parent', '00000',
    $q$select pg_temp.structure('00000000-0000-0000-0000-0000000cb002',
        '00000000-0000-0000-0000-0000000cb001')$q$);

select pg_temp.run('a leg of a leg is refused, so structures nest one level at most', '23514',
    $q$select pg_temp.structure('00000000-0000-0000-0000-0000000cb003',
        '00000000-0000-0000-0000-0000000cb002')$q$);

select pg_temp.run('a structure cannot be its own parent', '23514',
    $q$select pg_temp.structure('00000000-0000-0000-0000-0000000cb004',
        '00000000-0000-0000-0000-0000000cb004')$q$);

select pg_temp.run('a structure naming an unknown kind is refused', '23503',
    $q$select pg_temp.structure('00000000-0000-0000-0000-0000000cb005', null, 'Nonsense')$q$);

select pg_temp.run('a structure naming an unknown template is refused', '23503',
    $q$select pg_temp.structure('00000000-0000-0000-0000-0000000cb006', null, 'Strategy',
        'Nonsense')$q$);

select pg_temp.run('a structure resting on no template is written, as a package does', '00000',
    $q$select pg_temp.structure('00000000-0000-0000-0000-0000000cb007', null, 'Package', null)$q$);

select pg_temp.run('a structure naming an unknown parent is refused', '23503',
    $q$select pg_temp.structure('00000000-0000-0000-0000-0000000cb008',
        '00000000-0000-0000-0000-0000000cb0ff')$q$);

-- Informational: what an immutable row actually does under update and delete.
select pg_temp.run('update a structure (informational)', '?',
    $q$update ores_trading_structures_tbl set kind = 'Typed'
       where id = '00000000-0000-0000-0000-0000000cb001'$q$);

select pg_temp.run('delete a structure (informational)', '?',
    $q$delete from ores_trading_structures_tbl
       where id = '00000000-0000-0000-0000-0000000cb001'$q$);

select pg_temp.expect('the two structures written by the success cases are present',
    (select count(*) from ores_trading_structures_tbl
     where id in ('00000000-0000-0000-0000-0000000cb001'::uuid,
                  '00000000-0000-0000-0000-0000000cb002'::uuid,
                  '00000000-0000-0000-0000-0000000cb007'::uuid)) = 3);

select pg_temp.expect('the parent is recorded on the leg',
    (select parent_structure_id from ores_trading_structures_tbl
     where id = '00000000-0000-0000-0000-0000000cb002') =
    '00000000-0000-0000-0000-0000000cb001'::uuid);
"""

REPORT = """
\\pset footer off
select rpad(n::text, 3) || rpad(case when kind = 'read' then 'read' else 'case' end, 6)
       || rpad(case when ok is null then 'INFO' when ok then 'PASS' else 'FAIL' end, 6)
       || rpad(got, 8) || name
from probe_results order by n;

select 'total=' || count(*) || ' passed=' || count(*) filter (where ok)
       || ' failed=' || count(*) filter (where ok = false)
       || ' informational=' || count(*) filter (where ok is null)
from probe_results;
"""


def main() -> int:
    sql = "\n".join([
        "begin;",
        "select set_config('app.current_tenant_id', "
        "ores_utility_system_tenant_id_fn()::text, true);",
        FIXTURE,
        "select set_config('app.visible_party_ids', "
        "(select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);",
        HARNESS,
        CASES,
        REPORT,
        "rollback;",
    ])

    env = dict(os.environ)
    env["PGPASSWORD"] = ENV["ORES_TEST_DB_DDL_PASSWORD"]
    proc = subprocess.run(DSN, input=sql, text=True, capture_output=True,
                          cwd=REPO_ROOT, env=env)

    if proc.stdout:
        print(proc.stdout)
    if proc.returncode != 0:
        print("psql failed:", file=sys.stderr)
        print(proc.stderr, file=sys.stderr)
        return proc.returncode
    if proc.stderr.strip():
        print("stderr:", proc.stderr, file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
