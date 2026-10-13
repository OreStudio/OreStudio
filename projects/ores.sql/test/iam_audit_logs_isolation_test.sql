/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
/**
 * pgTAP tests for the tenant isolation of the authentication events and the run
 * token issues.
 *
 * Both tables name accounts, sessions, source addresses and the runs a grant
 * served, and carry their tenant as text. A tenant reads and writes only its own
 * rows. A row that names another tenant, or none, is refused to a tenant, and
 * the system tenant reads only its own like any other tenant: a row that names
 * no tenant is the system tenant's, which is how the log classes write it.
 *
 * The other tenant is one people set up, not an automation tenant. The
 * assertions about it need such a tenant to exist, so provision the Acme dataset
 * first, as the test environments do.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/iam_audit_logs_isolation_test.sql
 */

begin;

select plan(12);

create temporary table t_other on commit drop as
select id from ores_iam_tenants_tbl
where id <> ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
  and status = 'active'
  and type <> 'automation'
order by id
limit 1;

select ok(exists (select 1 from t_other), 'A tenant that people set up exists');

-- A tenant writes its own events.
select set_config('app.current_tenant_id', (select id::text from t_other), true);

select lives_ok(
    format($$insert into ores_iam_auth_events_tbl (id, event_time, tenant_id, event_type, username)
             values (gen_random_uuid()::text, now(), %L, 'login_success', 'probe')$$,
           (select id::text from t_other)),
    'A tenant writes an auth event of its own');

select lives_ok(
    format($$insert into ores_iam_run_token_issues_tbl (id, issued_at, tenant_id, outcome)
             values (gen_random_uuid()::text, now(), %L, 'issued')$$,
           (select id::text from t_other)),
    'A tenant writes a run token issue of its own');

-- A tenant does not write another's, the system tenant's, or none.
select throws_ok(
    format($$insert into ores_iam_auth_events_tbl (id, event_time, tenant_id, event_type)
             values (gen_random_uuid()::text, now(), %L, 'login_failure')$$,
           ores_utility_system_tenant_id_fn()::text),
    '42501', null,
    'A tenant does not write an auth event of the system tenant');

select throws_ok(
    $$insert into ores_iam_auth_events_tbl (id, event_time, tenant_id, event_type)
      values (gen_random_uuid()::text, now(), '', 'login_failure')$$,
    '42501', null,
    'A tenant does not write an auth event that names no tenant');

select throws_ok(
    format($$insert into ores_iam_run_token_issues_tbl (id, issued_at, tenant_id, outcome)
             values (gen_random_uuid()::text, now(), %L, 'refused')$$,
           ores_utility_system_tenant_id_fn()::text),
    '42501', null,
    'A tenant does not write a run token issue of the system tenant');

-- The system tenant writes its own, and the platform's events are its.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select lives_ok(
    format($$insert into ores_iam_auth_events_tbl (id, event_time, tenant_id, event_type, username)
             values (gen_random_uuid()::text, now(), %L, 'login_failure', 'platform_probe')$$,
           ores_utility_system_tenant_id_fn()::text),
    'The system tenant writes an auth event of its own');

select is(
    (select count(*) from ores_iam_auth_events_tbl
     where tenant_id = (select id::text from t_other)),
    0::bigint,
    'The system tenant does not read the events of another tenant');

select is(
    (select count(*) from ores_iam_run_token_issues_tbl
     where tenant_id = (select id::text from t_other)),
    0::bigint,
    'The system tenant does not read the run token issues of another tenant');

-- A tenant reads only its own.
select set_config('app.current_tenant_id', (select id::text from t_other), true);

select is(
    (select count(*) from ores_iam_auth_events_tbl where username = 'platform_probe'),
    0::bigint,
    'A tenant does not read an event of the system tenant');

select is(
    (select count(*) from ores_iam_auth_events_tbl where username = 'probe'),
    1::bigint,
    'A tenant reads its own events');

select is(
    (select count(*) from ores_iam_run_token_issues_tbl),
    1::bigint,
    'A tenant reads its own run token issues');

select * from finish();

rollback;
