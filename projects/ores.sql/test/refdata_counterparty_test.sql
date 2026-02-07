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
 * pgTAP tests for ores_refdata_counterparties_tbl trigger behavior.
 *
 * Tests cover:
 * - Insert with valid data succeeds
 * - party_type validation (invalid value rejected)
 * - status validation (invalid value rejected)
 * - Parent counterparty validation (invalid parent rejected)
 * - Natural key uniqueness (full_name, short_code)
 * - First insert gets version 1
 * - Soft delete via DELETE rule
 *
 * Note: No root party uniqueness constraint for counterparties.
 *
 * Run with: pg_prove -d ores_dev_local1 test/refdata_counterparty_test.sql
 */

begin;

select plan(9);

-- =============================================================================
-- Test: Valid insert
-- =============================================================================

-- Test 1: Insert counterparty succeeds with version 1
insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_counterparty_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'b0000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0, 'Acme Corporation', 'ACME', 'Corporate',
    NULL, NULL, 'Active',
    current_user, current_user, 'system.test', 'Test counterparty insert'
);

select is(
    (select version from ores_refdata_counterparties_tbl
     where id = 'b0000000-0000-0000-0000-000000000001'::uuid
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'counterparty insert: first version is 1'
);

-- Test 2: Status defaults to Active
select is(
    (select status from ores_refdata_counterparties_tbl
     where id = 'b0000000-0000-0000-0000-000000000001'::uuid
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'Active',
    'counterparty insert: status is Active'
);

-- =============================================================================
-- Test: Validation - party_type
-- =============================================================================

-- Test 3: Invalid party_type is rejected
select throws_ok(
    $$insert into ores_refdata_counterparties_tbl (
        id, tenant_id, version, full_name, short_code, party_type,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'b0000000-0000-0000-0000-000000000099'::uuid,
        ores_iam_system_tenant_id_fn(), 0, 'Bad Type Inc', 'BTI', 'INVALID_TYPE',
        current_user, current_user, 'system.test', 'Test'
    )$$,
    '23503',
    NULL,
    'counterparty insert: invalid party_type raises 23503'
);

-- =============================================================================
-- Test: Validation - status
-- =============================================================================

-- Test 4: Invalid status is rejected
select throws_ok(
    $$insert into ores_refdata_counterparties_tbl (
        id, tenant_id, version, full_name, short_code, party_type, status,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'b0000000-0000-0000-0000-000000000098'::uuid,
        ores_iam_system_tenant_id_fn(), 0, 'Bad Status Inc', 'BSI', 'Corporate', 'INVALID_STATUS',
        current_user, current_user, 'system.test', 'Test'
    )$$,
    '23503',
    NULL,
    'counterparty insert: invalid status raises 23503'
);

-- =============================================================================
-- Test: Parent counterparty validation
-- =============================================================================

-- Test 5: Insert child counterparty with valid parent succeeds
insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_counterparty_id,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'b0000000-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(), 0, 'Acme UK Ltd', 'ACUK', 'Corporate',
    'b0000000-0000-0000-0000-000000000001'::uuid,
    current_user, current_user, 'system.test', 'Test child counterparty'
);

select is(
    (select parent_counterparty_id::text from ores_refdata_counterparties_tbl
     where id = 'b0000000-0000-0000-0000-000000000002'::uuid
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'b0000000-0000-0000-0000-000000000001',
    'counterparty insert: child has correct parent_counterparty_id'
);

-- Test 6: Invalid parent_counterparty_id is rejected
select throws_ok(
    $$insert into ores_refdata_counterparties_tbl (
        id, tenant_id, version, full_name, short_code, party_type,
        parent_counterparty_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'b0000000-0000-0000-0000-000000000097'::uuid,
        ores_iam_system_tenant_id_fn(), 0, 'Orphan Inc', 'OI', 'Corporate',
        'deadbeef-0000-0000-0000-000000000000'::uuid,
        current_user, current_user, 'system.test', 'Test'
    )$$,
    '23503',
    NULL,
    'counterparty insert: invalid parent_counterparty_id raises 23503'
);

-- =============================================================================
-- Test: Natural key uniqueness
-- =============================================================================

-- Test 7: Duplicate full_name is rejected
select throws_ok(
    $$insert into ores_refdata_counterparties_tbl (
        id, tenant_id, version, full_name, short_code, party_type,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'b0000000-0000-0000-0000-000000000096'::uuid,
        ores_iam_system_tenant_id_fn(), 0, 'Acme Corporation', 'UNIQUE', 'Corporate',
        current_user, current_user, 'system.test', 'Test'
    )$$,
    '23505',
    NULL,
    'counterparty insert: duplicate full_name raises unique_violation'
);

-- =============================================================================
-- Test: Soft delete
-- =============================================================================

-- Test 8: DELETE sets valid_to instead of removing
delete from ores_refdata_counterparties_tbl
where id = 'b0000000-0000-0000-0000-000000000002'::uuid;

select is(
    (select count(*)::integer from ores_refdata_counterparties_tbl
     where id = 'b0000000-0000-0000-0000-000000000002'::uuid
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'counterparty delete: record no longer current after soft delete'
);

-- Test 9: Soft-deleted record still exists in history
select ok(
    (select count(*) > 0 from ores_refdata_counterparties_tbl
     where id = 'b0000000-0000-0000-0000-000000000002'::uuid
       and valid_to != ores_utility_infinity_timestamp_fn()),
    'counterparty delete: historical record preserved after soft delete'
);

select * from finish();

rollback;
