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
 * pgTAP tests for refdata validation functions.
 *
 * Tests cover:
 * - Default value behavior for null/empty input
 * - Valid value pass-through
 * - Invalid value rejection
 *
 * Run with: pg_prove -d ores_dev_local1 test/refdata_validation_test.sql
 */

begin;

select plan(12);

-- =============================================================================
-- Test: ores_refdata_validate_rounding_type_fn
-- =============================================================================

-- Test 1: NULL returns default 'Closest'
select is(
    ores_refdata_validate_rounding_type_fn(ores_iam_system_tenant_id_fn(), NULL),
    'Closest',
    'rounding_type: NULL returns default Closest'
);

-- Test 2: Empty string returns default 'Closest'
select is(
    ores_refdata_validate_rounding_type_fn(ores_iam_system_tenant_id_fn(), ''),
    'Closest',
    'rounding_type: empty string returns default Closest'
);

-- Test 3: Valid value 'Up' returns itself
select is(
    ores_refdata_validate_rounding_type_fn(ores_iam_system_tenant_id_fn(), 'Up'),
    'Up',
    'rounding_type: valid value Up returns itself'
);

-- Test 4: Valid value 'Down' returns itself
select is(
    ores_refdata_validate_rounding_type_fn(ores_iam_system_tenant_id_fn(), 'Down'),
    'Down',
    'rounding_type: valid value Down returns itself'
);

-- Test 5: Valid value 'Floor' returns itself
select is(
    ores_refdata_validate_rounding_type_fn(ores_iam_system_tenant_id_fn(), 'Floor'),
    'Floor',
    'rounding_type: valid value Floor returns itself'
);

-- Test 6: Valid value 'Ceiling' returns itself
select is(
    ores_refdata_validate_rounding_type_fn(ores_iam_system_tenant_id_fn(), 'Ceiling'),
    'Ceiling',
    'rounding_type: valid value Ceiling returns itself'
);

-- Test 7: Invalid value raises exception
select throws_ok(
    $$select ores_refdata_validate_rounding_type_fn(ores_iam_system_tenant_id_fn(), 'INVALID')$$,
    '23503',
    NULL,
    'rounding_type: invalid value raises 23503 exception'
);

-- =============================================================================
-- Test: ores_dq_validate_change_reason_fn
-- =============================================================================

-- Test 8: NULL returns default 'system.new_record'
select is(
    ores_dq_validate_change_reason_fn(ores_iam_system_tenant_id_fn(), NULL),
    'system.new_record',
    'change_reason: NULL returns default system.new_record'
);

-- Test 9: Empty string returns default 'system.new_record'
select is(
    ores_dq_validate_change_reason_fn(ores_iam_system_tenant_id_fn(), ''),
    'system.new_record',
    'change_reason: empty string returns default system.new_record'
);

-- Test 10: Valid value 'system.initial_load' returns itself
select is(
    ores_dq_validate_change_reason_fn(ores_iam_system_tenant_id_fn(), 'system.initial_load'),
    'system.initial_load',
    'change_reason: valid value system.initial_load returns itself'
);

-- Test 11: Valid value 'common.rectification' returns itself
select is(
    ores_dq_validate_change_reason_fn(ores_iam_system_tenant_id_fn(), 'common.rectification'),
    'common.rectification',
    'change_reason: valid value common.rectification returns itself'
);

-- Test 12: Invalid value raises exception
select throws_ok(
    $$select ores_dq_validate_change_reason_fn(ores_iam_system_tenant_id_fn(), 'INVALID_REASON')$$,
    '23503',
    NULL,
    'change_reason: invalid value raises 23503 exception'
);

select * from finish();

rollback;
