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
 * pgTAP tests for ores_refdata_currencies_tbl trigger behavior.
 *
 * Tests cover:
 * - Insert with null rounding_type defaults to 'Closest'
 * - Insert with null change_reason_code defaults to 'system.new_record'
 * - Insert with invalid rounding_type is rejected
 * - Insert with invalid change_reason_code is rejected
 * - First insert gets version 1
 *
 * Note: Version increment tests require separate transactions and are
 * tested manually or via integration tests.
 *
 * Run with: pg_prove -d ores_dev_local1 test/refdata_currencies_test.sql
 */

begin;

select plan(5);

-- =============================================================================
-- Test: Insert with default values
-- =============================================================================

-- Test 1: Insert with NULL rounding_type defaults to 'Closest'
insert into ores_refdata_currencies_tbl (
    tenant_id, iso_code, version, name, numeric_code, symbol,
    fraction_symbol, fractions_per_unit, rounding_type, rounding_precision,
    format, asset_class, market_tier, modified_by, performed_by, change_reason_code, change_commentary
) values (
    ores_iam_system_tenant_id_fn(), 'TST', 0, 'Test Currency', '999', 'T',
    'c', 100, NULL, 2,
    'T#,##0.00', 'fiat', 'test', current_user, current_user, 'system.test', 'Test insert'
);

select is(
    (select rounding_type from ores_refdata_currencies_tbl
     where iso_code = 'TST' and valid_to = ores_utility_infinity_timestamp_fn()),
    'Closest',
    'currency insert: NULL rounding_type defaults to Closest'
);

-- Test 2: Insert with NULL change_reason_code defaults to 'system.new_record'
insert into ores_refdata_currencies_tbl (
    tenant_id, iso_code, version, name, numeric_code, symbol,
    fraction_symbol, fractions_per_unit, rounding_type, rounding_precision,
    format, asset_class, market_tier, modified_by, performed_by, change_reason_code, change_commentary
) values (
    ores_iam_system_tenant_id_fn(), 'TS2', 0, 'Test Currency 2', '998', 'T',
    'c', 100, 'Up', 2,
    'T#,##0.00', 'fiat', 'test', current_user, current_user, NULL, 'Test insert'
);

select is(
    (select change_reason_code from ores_refdata_currencies_tbl
     where iso_code = 'TS2' and valid_to = ores_utility_infinity_timestamp_fn()),
    'system.new_record',
    'currency insert: NULL change_reason_code defaults to system.new_record'
);

-- Test 3: First insert gets version 1
select is(
    (select version from ores_refdata_currencies_tbl
     where iso_code = 'TST' and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'currency insert: first version is 1'
);

-- =============================================================================
-- Test: Invalid values are rejected
-- =============================================================================

-- Test 4: Insert with invalid rounding_type is rejected
select throws_ok(
    $$insert into ores_refdata_currencies_tbl (
        tenant_id, iso_code, version, name, numeric_code, symbol,
        fraction_symbol, fractions_per_unit, rounding_type, rounding_precision,
        format, asset_class, market_tier, modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        ores_iam_system_tenant_id_fn(), 'BAD', 0, 'Bad Currency', '997', 'B',
        'c', 100, 'INVALID_ROUNDING', 2,
        'B#,##0.00', 'fiat', 'test', current_user, current_user, 'system.test', 'Test'
    )$$,
    '23503',
    NULL,
    'currency insert: invalid rounding_type raises 23503'
);

-- Test 5: Insert with invalid change_reason_code is rejected
select throws_ok(
    $$insert into ores_refdata_currencies_tbl (
        tenant_id, iso_code, version, name, numeric_code, symbol,
        fraction_symbol, fractions_per_unit, rounding_type, rounding_precision,
        format, asset_class, market_tier, modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        ores_iam_system_tenant_id_fn(), 'BD2', 0, 'Bad Currency 2', '996', 'B',
        'c', 100, 'Closest', 2,
        'B#,##0.00', 'fiat', 'test', current_user, current_user, 'INVALID_REASON', 'Test'
    )$$,
    '23503',
    NULL,
    'currency insert: invalid change_reason_code raises 23503'
);

select * from finish();

rollback;
