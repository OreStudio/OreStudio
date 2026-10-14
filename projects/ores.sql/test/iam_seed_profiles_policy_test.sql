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
 * pgTAP tests for who may read and write the seed profiles.
 *
 * The seed profiles, their steps and their parameters are the platform's own
 * configuration, and every row belongs to the system tenant. Every tenant reads
 * them, because a tenant is provisioned from a profile. Only the system tenant
 * writes them: a tenant that inserts, changes or removes a row is refused or
 * finds no row to change.
 *
 * The assertions about a tenant need one that people set up, so provision the
 * Acme dataset first, as the test environments do.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/iam_seed_profiles_policy_test.sql
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
    'production', 'test_seed_profile_policy', 'Seed Profile Policy Fixture',
    'test_seed_profile_policy');

select ok(exists (select 1 from t_other), 'A tenant that people set up exists');

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_before on commit drop as
select
    (select count(*) from ores_iam_seed_profiles_tbl where valid_to = ores_utility_infinity_timestamp_fn()) as profiles,
    (select count(*) from ores_iam_seed_profile_steps_tbl where valid_to = ores_utility_infinity_timestamp_fn()) as steps,
    (select count(*) from ores_iam_seed_profile_parameters_tbl where valid_to = ores_utility_infinity_timestamp_fn()) as parameters;

select cmp_ok((select profiles from t_before), '>', 0::bigint,
    'The system tenant reads the seed profiles');

select set_config('app.current_tenant_id', (select id::text from t_other), true);

select is(
    (select count(*) from ores_iam_seed_profiles_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select profiles from t_before),
    'A tenant reads every seed profile');

select is(
    (select count(*) from ores_iam_seed_profile_steps_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select steps from t_before),
    'A tenant reads every seed profile step');

select throws_ok(
    $$insert into ores_iam_seed_profiles_tbl (
          id, version, code, name, summary, audience,
          modified_by, performed_by, change_reason_code, change_commentary,
          valid_from, valid_to)
      values (gen_random_uuid(), 1, 'tenant_probe', 'Probe', 'Probe', 'Probe',
          'super_admin', 'super_admin', 'system.new_record', 'probe',
          now(), ores_utility_infinity_timestamp_fn())$$,
    '42501', null,
    'A tenant does not add a seed profile');

delete from ores_iam_seed_profiles_tbl;
delete from ores_iam_seed_profile_steps_tbl;
delete from ores_iam_seed_profile_parameters_tbl;

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*) from ores_iam_seed_profiles_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select profiles from t_before),
    'A tenant does not remove a seed profile');

select is(
    (select count(*) from ores_iam_seed_profile_steps_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select steps from t_before),
    'A tenant does not remove a seed profile step');

select is(
    (select count(*) from ores_iam_seed_profile_parameters_tbl where valid_to = ores_utility_infinity_timestamp_fn()),
    (select parameters from t_before),
    'A tenant does not remove a seed profile parameter');

select * from finish();

rollback;
