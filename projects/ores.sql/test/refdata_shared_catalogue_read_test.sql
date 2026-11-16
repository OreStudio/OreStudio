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
 * pgTAP tests for a catalogue every tenant reads and no tenant writes.
 *
 * A vocabulary the platform publishes lives in the system tenant, and a tenant
 * reads it there: the select policy admits the caller's rows and the system
 * tenant's, and the write policy admits the caller's rows alone. The second
 * half is what keeps the copy single. A write policy that admitted the shared
 * rows would let a tenant move a platform row into its own tenant, or close it
 * for everyone, and no reader would then be authoritative.
 *
 * The six facts are asserted on the tenors, a vocabulary that holds both kinds
 * of row because a tenant may add a tenor of its own:
 *
 * - a tenant reads the system tenant's rows;
 * - a tenant reads no other tenant's rows;
 * - a tenant changes no shared row;
 * - a tenant removes no shared row;
 * - a tenant writes no row for the system tenant;
 * - a tenant writes and reads its own row.
 *
 * The other tenant is one people set up, so the suite provisions it and lets
 * the transaction roll it back.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/refdata_shared_catalogue_read_test.sql
 */

begin;

select plan(6);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_tenant on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_shared_catalogue', 'Shared Catalogue Fixture',
    'test_shared_catalogue');

create temporary table t_shared on commit drop as
select code from ores_refdata_tenors_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
order by code
limit 1;

select set_config('app.current_tenant_id', (select id::text from t_tenant), true);

select is(
    (select count(*) from ores_refdata_tenors_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()) > 0,
    true,
    'a tenant reads the tenors the system tenant publishes');

-- A tenor of the tenant's own, which the tenant next to it must not see.
insert into ores_refdata_tenors_tbl (
    code, tenant_id, version, display_name, description, sort_order,
    kind, unit, multiplier,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'ZZ_SHARED_PROBE', ores_iam_current_tenant_id_fn(), 0, 'ZZ probe', null, 0,
    'PERIOD', 'MONTH', 1,
    current_user, current_user, 'system.test', 'Shared catalogue probe');

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_other on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_shared_catalogue_other', 'Shared Catalogue Other',
    'test_shared_catalogue_other');

select set_config('app.current_tenant_id', (select id::text from t_other), true);

select is(
    (select count(*) from ores_refdata_tenors_tbl where code = 'ZZ_SHARED_PROBE'),
    0::bigint,
    'a tenant reads no other tenant''s row');

-- A write that names the system tenant is a write into the caller's own
-- tenant: the trigger resolves the tenant it is given, so a caller cannot add a
-- row to the shared copy.
insert into ores_refdata_tenors_tbl (
    code, tenant_id, version, display_name, description, sort_order,
    kind, unit, multiplier,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'ZZ_FORGED_PROBE', ores_utility_system_tenant_id_fn(), 0, 'ZZ forged', null, 0,
    'PERIOD', 'MONTH', 1,
    current_user, current_user, 'system.test', 'Shared catalogue probe');

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*) from ores_refdata_tenors_tbl where code = 'ZZ_FORGED_PROBE'),
    0::bigint,
    'a write that names the system tenant lands in the caller''s own tenant');

select set_config('app.current_tenant_id', (select id::text from t_tenant), true);

create temporary table t_changed on commit drop as
with changed as (
    update ores_refdata_tenors_tbl set tenant_id = ores_iam_current_tenant_id_fn()
    where tenant_id = ores_utility_system_tenant_id_fn()
      and code = (select code from t_shared)
    returning 1)
select count(*)::int as n from changed;

select is(
    (select n from t_changed),
    0,
    'a tenant moves no shared row into its own tenant');

delete from ores_refdata_tenors_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and code = (select code from t_shared);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*) from ores_refdata_tenors_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = (select code from t_shared)
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'a tenant closes no shared row');

select set_config('app.current_tenant_id', (select id::text from t_tenant), true);

select is(
    (select count(*) from ores_refdata_tenors_tbl where code = 'ZZ_SHARED_PROBE'),
    1::bigint,
    'a tenant reads the row it wrote');

select * from finish();

rollback;
