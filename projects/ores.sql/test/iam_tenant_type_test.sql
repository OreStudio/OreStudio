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
 * pgTAP tests for ores_iam_validate_tenant_type_fn.
 *
 * Tests cover:
 * - NULL value raises exception (no default)
 * - Empty string raises exception (no default)
 * - Valid values pass through
 * - Invalid value raises exception
 *
 * Run with: pg_prove -d ores_dev_local1 test/iam_tenant_type_test.sql
 */

begin;

select plan(6);

-- =============================================================================
-- Test: NULL and empty values raise exceptions (no default)
-- =============================================================================

-- Test 1: NULL raises 23502 exception
select throws_ok(
    $$select ores_iam_validate_tenant_type_fn(ores_iam_system_tenant_id_fn(), NULL)$$,
    '23502',
    NULL,
    'tenant_type: NULL raises 23502 exception'
);

-- Test 2: Empty string raises 23502 exception
select throws_ok(
    $$select ores_iam_validate_tenant_type_fn(ores_iam_system_tenant_id_fn(), '')$$,
    '23502',
    NULL,
    'tenant_type: empty string raises 23502 exception'
);

-- =============================================================================
-- Test: Valid values pass through
-- =============================================================================

-- Test 3: Valid value 'system' returns itself
select is(
    ores_iam_validate_tenant_type_fn(ores_iam_system_tenant_id_fn(), 'system'),
    'system',
    'tenant_type: valid value system returns itself'
);

-- Test 4: Valid value 'production' returns itself
select is(
    ores_iam_validate_tenant_type_fn(ores_iam_system_tenant_id_fn(), 'production'),
    'production',
    'tenant_type: valid value production returns itself'
);

-- Test 5: Valid value 'evaluation' returns itself
select is(
    ores_iam_validate_tenant_type_fn(ores_iam_system_tenant_id_fn(), 'evaluation'),
    'evaluation',
    'tenant_type: valid value evaluation returns itself'
);

-- =============================================================================
-- Test: Invalid values raise exception
-- =============================================================================

-- Test 6: Invalid value raises 23503 exception
select throws_ok(
    $$select ores_iam_validate_tenant_type_fn(ores_iam_system_tenant_id_fn(), 'INVALID_TYPE')$$,
    '23503',
    NULL,
    'tenant_type: invalid value raises 23503 exception'
);

select * from finish();

rollback;
