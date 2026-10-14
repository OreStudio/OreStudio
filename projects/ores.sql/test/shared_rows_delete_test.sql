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
 * pgTAP tests that a tenant cannot remove a row the system tenant shares.
 *
 * A delete is rewritten by a rule into an update that closes the row, and the
 * rule runs with the owner's rights, which row-level security does not bind. A
 * tenant that reads a shared row can then close it unless the rule or the
 * policies state the limit. The compute apps, app versions, app version
 * platforms and platforms are shared by the system tenant and read by every
 * tenant.
 *
 * The assertions about a tenant need one that people set up, so provision the
 * Acme dataset first, as the test environments do.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/shared_rows_delete_test.sql
 */

begin;

select plan(8);

-- The assertions need a tenant that people set up, and a rebuilt database has
-- none: provisioning is a runtime act, not part of the build. Provision one
-- here and let the transaction roll it back, so the suite owns its fixture
-- instead of depending on out-of-band setup.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_other on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_shared_rows_delete', 'Shared Rows Delete Fixture',
    'test_shared_rows_delete');

select ok(exists (select 1 from t_other), 'A tenant that people set up exists');

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_before on commit drop as
select
    (select count(*) from ores_compute_apps_tbl where valid_to = ores_utility_infinity_timestamp_fn()) as apps,
    (select count(*) from ores_compute_app_versions_tbl where valid_to = ores_utility_infinity_timestamp_fn()) as versions,
    (select count(*) from ores_compute_app_version_platforms_tbl where valid_to = ores_utility_infinity_timestamp_fn()) as links,
    (select count(*) from ores_compute_platforms_tbl where valid_to = ores_utility_infinity_timestamp_fn()) as platforms;

select cmp_ok((select apps from t_before), '>', 0::bigint, 'The system tenant has shared apps');

select set_config('app.current_tenant_id', (select id::text from t_other), true);

delete from ores_compute_apps_tbl;
delete from ores_compute_app_versions_tbl;
delete from ores_compute_app_version_platforms_tbl;
delete from ores_compute_platforms_tbl;

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is((select count(*) from ores_compute_apps_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select apps from t_before), 'A tenant does not remove a shared app');

select is((select count(*) from ores_compute_app_versions_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select versions from t_before), 'A tenant does not remove a shared app version');

select is((select count(*) from ores_compute_app_version_platforms_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select links from t_before), 'A tenant does not remove a shared app version platform');

select is((select count(*) from ores_compute_platforms_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select platforms from t_before), 'A tenant does not remove a shared platform');

select set_config('app.current_tenant_id', (select id::text from t_other), true);

select is((select count(*) from ores_compute_apps_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select apps from t_before), 'A tenant still reads the shared apps');

select is((select count(*) from ores_compute_platforms_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select platforms from t_before), 'A tenant still reads the shared platforms');

select * from finish();

rollback;
