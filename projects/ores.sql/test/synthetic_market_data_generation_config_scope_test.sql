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
 * pgTAP tests for the market_data_generation_config scope/binding_mode
 * conflict-rejection insert trigger and the scope/nullability check
 * constraint.
 *
 * Run with: pg_prove -d ores_dev_local1 test/synthetic_market_data_generation_config_scope_test.sql
 */

begin;

select plan(5);

-- =============================================================================
-- Test 1: A tenant-scope bound config followed by a conflicting
-- party-scope bound config for the same tenant is rejected.
-- =============================================================================

insert into ores_synthetic_market_data_generation_configs_tbl
(id, tenant_id, version, party_id, scope, binding_mode, name, description, enabled, dataset_id, modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, null, 'tenant', 'bound', 'scope_test_tenant_wide', 'test', true, null, 'ores_prime_origin_ddl_user', 'ores_prime_origin_ddl_user', 'system.initial_load', 'seed', now(), 'infinity');

select throws_ok(
    $$insert into ores_synthetic_market_data_generation_configs_tbl
      (id, tenant_id, version, party_id, scope, binding_mode, name, description, enabled, dataset_id, modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
      values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, gen_random_uuid(), 'party', 'bound', 'scope_test_party_narrower', 'test', true, null, 'ores_prime_origin_ddl_user', 'ores_prime_origin_ddl_user', 'system.initial_load', 'seed', now(), 'infinity')$$,
    '23514',
    NULL,
    'party-scope bound config conflicting with an active tenant-scope bound config is rejected'
);

-- =============================================================================
-- Test 2: A sandboxed config for the same tenant/party is never blocked
-- by the conflict guard -- sandbox never participates.
-- =============================================================================

select lives_ok(
    $$insert into ores_synthetic_market_data_generation_configs_tbl
      (id, tenant_id, version, party_id, scope, binding_mode, name, description, enabled, dataset_id, modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
      values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, gen_random_uuid(), 'party', 'sandboxed', 'scope_test_party_sandbox', 'test', true, null, 'ores_prime_origin_ddl_user', 'ores_prime_origin_ddl_user', 'system.initial_load', 'seed', now(), 'infinity')$$,
    'sandboxed config never conflicts with an active bound config at any scope'
);

-- =============================================================================
-- Test 3: A second, disabled bound config at the same scope is allowed
-- -- only *enabled* bound configs participate in the conflict check.
-- =============================================================================

select lives_ok(
    $$insert into ores_synthetic_market_data_generation_configs_tbl
      (id, tenant_id, version, party_id, scope, binding_mode, name, description, enabled, dataset_id, modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
      values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, null, 'tenant', 'bound', 'scope_test_tenant_wide_disabled', 'test', false, null, 'ores_prime_origin_ddl_user', 'ores_prime_origin_ddl_user', 'system.initial_load', 'seed', now(), 'infinity')$$,
    'a disabled bound config at the same scope does not conflict'
);

-- =============================================================================
-- Test 4: The scope/nullability check constraint rejects a malformed
-- row (scope = party with a null party_id) that would otherwise bypass
-- the conflict guard's NULL-unsafe party_id = party_id comparison.
-- binding_mode is sandboxed here so the conflict-guard trigger (which
-- would also raise 23514, but for a different reason) never fires --
-- isolating the check constraint as the only possible cause.
-- =============================================================================

select throws_ok(
    $$insert into ores_synthetic_market_data_generation_configs_tbl
      (id, tenant_id, version, party_id, scope, binding_mode, name, description, enabled, dataset_id, modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
      values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, null, 'party', 'sandboxed', 'scope_test_malformed_party', 'test', true, null, 'ores_prime_origin_ddl_user', 'ores_prime_origin_ddl_user', 'system.initial_load', 'seed', now(), 'infinity')$$,
    '23514',
    NULL,
    'scope=party with a null party_id violates the scope/nullability check constraint'
);

select throws_ok(
    $$insert into ores_synthetic_market_data_generation_configs_tbl
      (id, tenant_id, version, party_id, scope, binding_mode, name, description, enabled, dataset_id, modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to)
      values (gen_random_uuid(), null, 0, gen_random_uuid(), 'system', 'sandboxed', 'scope_test_malformed_system', 'test', true, null, 'ores_prime_origin_ddl_user', 'ores_prime_origin_ddl_user', 'system.initial_load', 'seed', now(), 'infinity')$$,
    '23514',
    NULL,
    'scope=system with a non-null party_id violates the scope/nullability check constraint'
);

select * from finish();

rollback;
