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
 * pgTAP tests for the shared accounts of the system tenant.
 *
 * The service accounts and the super administrator belong to the system tenant,
 * and the history of every tenant names them as the author of an entry. A system
 * tenant row with shared set is readable by every tenant, whoever the actor is,
 * and no other system tenant row is. A tenant never writes a shared row. A
 * record that names an account checks that it exists through
 * ores_iam_account_exists_fn, which answers for the session's tenant and for the
 * shared accounts of the system tenant, and for no other tenant.
 *
 * The other tenant is one people set up, not an automation tenant.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/iam_accounts_shared_visibility_test.sql
 */

begin;

select plan(10);

create temporary table t_other on commit drop as
select id from ores_iam_tenants_tbl
where id <> ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
  and status = 'active'
  and type <> 'automation'
order by id
limit 1;

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A system tenant account that is not shared: a person who administers the
-- platform and has not been made visible to the tenants.
insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, email, shared,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-0000000000a1'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'user', 'shared_probe_hidden', 'hidden@system.ores', false,
    current_user, current_user, 'system.test', 'Shared visibility probe'
);

create temporary table t_service on commit drop as
select id from ores_iam_accounts_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and account_type = 'service'
  and shared
  and valid_to = ores_utility_infinity_timestamp_fn()
order by username
limit 1;

select set_config('app.current_tenant_id', coalesce((select id::text from t_other),
    ores_utility_system_tenant_id_fn()::text), true);
select set_config('app.current_actor', '', true);

-- The system tenant id in a write means the session tenant, so a tenant that
-- states the system tenant still writes into its own.
insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, email, shared,
    modified_by, performed_by, change_reason_code, change_commentary
) select
    'a0000000-0000-0000-0000-0000000000a2'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'service', 'shared_probe_forged', 'forged@system.ores', true,
    current_user, current_user, 'system.test', 'Shared visibility probe'
where exists (select 1 from t_other);

-- A data-modifying WITH must be top level, so the counts are taken first.
create temporary table t_changed on commit drop as
with changed as (
    update ores_iam_accounts_tbl set job_title = 'Changed by a tenant'
    where id = (select id from t_service)
    returning 1)
select count(*)::int as n from changed;

-- A delete is a rule that closes the row, so there is nothing to count: the
-- test reads the row back afterwards.
delete from ores_iam_accounts_tbl where id = (select id from t_service);

select case when not exists(select 1 from t_other)
    then skip('no other active tenant is provisioned', 10)
    else collect_tap(
        ok(exists(select 1 from ores_iam_accounts_tbl
                  where tenant_id = ores_utility_system_tenant_id_fn()
                    and account_type = 'service'),
           'a tenant reads the service accounts of the system tenant'),
        is((select count(*)::int from ores_iam_accounts_tbl
            where tenant_id = ores_utility_system_tenant_id_fn() and not shared),
           0,
           'a tenant reads no system tenant account that is not shared'),
        is((select count(*)::int from ores_iam_accounts_tbl
            where tenant_id not in (ores_utility_system_tenant_id_fn(),
                                    (select id from t_other))),
           0,
           'a tenant reads the accounts of no other tenant'),
        ok(ores_iam_account_exists_fn((select id from t_service)),
           'the existence check finds a shared account of the system tenant'),
        ok(not ores_iam_account_exists_fn('a0000000-0000-0000-0000-0000000000a1'::uuid),
           'the existence check does not find a system tenant account that is not shared'),
        ok(not ores_iam_account_exists_fn('a0000000-0000-0000-0000-0000000000ff'::uuid),
           'the existence check does not find an account that does not exist'),
        is((select n from t_changed), 0, 'a tenant changes no shared account'),
        ok(exists(select 1 from ores_iam_accounts_tbl
                  where id = (select id from t_service)
                    and valid_to = ores_utility_infinity_timestamp_fn()),
           'a tenant removes no shared account'),
        is((select tenant_id from ores_iam_accounts_tbl
            where id = 'a0000000-0000-0000-0000-0000000000a2'::uuid),
           ores_iam_current_tenant_id_fn(),
           'an account that a tenant writes for the system tenant lands in the tenant'),
        is((select count(*)::int from ores_iam_accounts_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and username = 'shared_probe_forged'),
           0,
           'a tenant writes no account for the system tenant'))
    end;

select * from finish();

rollback;
