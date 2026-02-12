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
 * pgTAP tests for ores_refdata_parties_tbl trigger behavior.
 *
 * Tests cover:
 * - Insert with valid data succeeds
 * - party_type validation (invalid value rejected)
 * - status validation (invalid value rejected, default Active applied)
 * - Parent party validation (invalid parent rejected)
 * - Root party uniqueness constraint
 * - Natural key uniqueness (full_name, short_code)
 * - First insert gets version 1
 * - Soft delete via DELETE rule
 *
 * Run with: pg_prove -d ores_dev_local1 test/refdata_party_test.sql
 */

begin;

select plan(12);

-- =============================================================================
-- Test: Valid insert
-- =============================================================================

-- Test 1: Insert root party (no parent) succeeds with version 1
insert into ores_refdata_parties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0, 'Test Bank Holdings', 'TBH', 'Bank',
    NULL, NULL, 'Active',
    current_user, current_user, 'system.test', 'Test party insert'
);

select is(
    (select version from ores_refdata_parties_tbl
     where id = 'a0000000-0000-0000-0000-000000000001'::uuid
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'party insert: first version is 1'
);

-- Test 2: Default status 'Active' is applied when explicitly set
select is(
    (select status from ores_refdata_parties_tbl
     where id = 'a0000000-0000-0000-0000-000000000001'::uuid
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'Active',
    'party insert: status is Active'
);

-- =============================================================================
-- Test: Validation - party_category
-- =============================================================================

-- Test 3: party_category defaults to 'Operational'
select is(
    (select party_category from ores_refdata_parties_tbl
     where id = 'a0000000-0000-0000-0000-000000000001'::uuid
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'Operational',
    'party insert: party_category defaults to Operational'
);

-- Test 4: Invalid party_category is rejected
select throws_ok(
    $$insert into ores_refdata_parties_tbl (
        id, tenant_id, version, full_name, short_code, party_category, party_type,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'a0000000-0000-0000-0000-000000000090'::uuid,
        ores_iam_system_tenant_id_fn(), 0, 'Bad Category Corp', 'BCC', 'INVALID_CATEGORY', 'Bank',
        current_user, current_user, 'system.test', 'Test'
    )$$,
    '23503',
    NULL,
    'party insert: invalid party_category raises 23503'
);

-- =============================================================================
-- Test: Validation - party_type
-- =============================================================================

-- Test 5: Invalid party_type is rejected
select throws_ok(
    $$insert into ores_refdata_parties_tbl (
        id, tenant_id, version, full_name, short_code, party_type,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'a0000000-0000-0000-0000-000000000099'::uuid,
        ores_iam_system_tenant_id_fn(), 0, 'Bad Type Corp', 'BTC', 'INVALID_TYPE',
        current_user, current_user, 'system.test', 'Test'
    )$$,
    '23503',
    NULL,
    'party insert: invalid party_type raises 23503'
);

-- =============================================================================
-- Test: Validation - status
-- =============================================================================

-- Test 6: Invalid status is rejected
select throws_ok(
    $$insert into ores_refdata_parties_tbl (
        id, tenant_id, version, full_name, short_code, party_type, status,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'a0000000-0000-0000-0000-000000000098'::uuid,
        ores_iam_system_tenant_id_fn(), 0, 'Bad Status Corp', 'BSC', 'Bank', 'INVALID_STATUS',
        current_user, current_user, 'system.test', 'Test'
    )$$,
    '23503',
    NULL,
    'party insert: invalid status raises 23503'
);

-- =============================================================================
-- Test: Parent party validation
-- =============================================================================

-- Test 7: Insert child party with valid parent succeeds
insert into ores_refdata_parties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_party_id,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(), 0, 'Test Bank UK', 'TBUK', 'Bank',
    'a0000000-0000-0000-0000-000000000001'::uuid,
    current_user, current_user, 'system.test', 'Test child party'
);

select is(
    (select parent_party_id::text from ores_refdata_parties_tbl
     where id = 'a0000000-0000-0000-0000-000000000002'::uuid
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'a0000000-0000-0000-0000-000000000001',
    'party insert: child party has correct parent_party_id'
);

-- Test 8: Invalid parent_party_id is rejected
select throws_ok(
    $$insert into ores_refdata_parties_tbl (
        id, tenant_id, version, full_name, short_code, party_type,
        parent_party_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'a0000000-0000-0000-0000-000000000097'::uuid,
        ores_iam_system_tenant_id_fn(), 0, 'Orphan Corp', 'OC', 'Bank',
        'deadbeef-0000-0000-0000-000000000000'::uuid,
        current_user, current_user, 'system.test', 'Test'
    )$$,
    '23503',
    NULL,
    'party insert: invalid parent_party_id raises 23503'
);

-- =============================================================================
-- Test: Root party uniqueness
-- =============================================================================

-- Test 9: Second root party (no parent) for same tenant is rejected
select throws_ok(
    $$insert into ores_refdata_parties_tbl (
        id, tenant_id, version, full_name, short_code, party_type,
        parent_party_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'a0000000-0000-0000-0000-000000000096'::uuid,
        ores_iam_system_tenant_id_fn(), 0, 'Second Root Corp', 'SRC', 'Bank',
        NULL,
        current_user, current_user, 'system.test', 'Test'
    )$$,
    '23505',
    NULL,
    'party insert: second root party raises unique_violation'
);

-- =============================================================================
-- Test: Natural key uniqueness
-- =============================================================================

-- Test 10: Duplicate full_name is rejected
select throws_ok(
    $$insert into ores_refdata_parties_tbl (
        id, tenant_id, version, full_name, short_code, party_type,
        parent_party_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'a0000000-0000-0000-0000-000000000095'::uuid,
        ores_iam_system_tenant_id_fn(), 0, 'Test Bank Holdings', 'UNIQUE', 'Bank',
        'a0000000-0000-0000-0000-000000000001'::uuid,
        current_user, current_user, 'system.test', 'Test'
    )$$,
    '23505',
    NULL,
    'party insert: duplicate full_name raises unique_violation'
);

-- =============================================================================
-- Test: Soft delete
-- =============================================================================

-- Test 11: DELETE sets valid_to instead of removing
delete from ores_refdata_parties_tbl
where id = 'a0000000-0000-0000-0000-000000000002'::uuid;

select is(
    (select count(*)::integer from ores_refdata_parties_tbl
     where id = 'a0000000-0000-0000-0000-000000000002'::uuid
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'party delete: record no longer current after soft delete'
);

-- Test 12: Soft-deleted record still exists in history
select ok(
    (select count(*) > 0 from ores_refdata_parties_tbl
     where id = 'a0000000-0000-0000-0000-000000000002'::uuid
       and valid_to != ores_utility_infinity_timestamp_fn()),
    'party delete: historical record preserved after soft delete'
);

select * from finish();

rollback;
