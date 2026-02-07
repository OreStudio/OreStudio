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
 * pgTAP tests for party-related reference data validation functions.
 *
 * Tests cover all four validation functions:
 * - ores_refdata_validate_party_type_fn
 * - ores_refdata_validate_party_status_fn
 * - ores_refdata_validate_party_id_scheme_fn
 * - ores_refdata_validate_contact_type_fn
 *
 * Run with: pg_prove -d ores_dev_local1 test/refdata_party_reference_data_test.sql
 */

begin;

select plan(16);

-- =============================================================================
-- Test: ores_refdata_validate_party_type_fn
-- =============================================================================

-- Test 1: NULL raises exception (no default value)
select throws_ok(
    $$select ores_refdata_validate_party_type_fn(ores_iam_system_tenant_id_fn(), NULL)$$,
    '23502',
    NULL,
    'party_type: NULL raises 23502 exception'
);

-- Test 2: Empty string raises exception
select throws_ok(
    $$select ores_refdata_validate_party_type_fn(ores_iam_system_tenant_id_fn(), '')$$,
    '23502',
    NULL,
    'party_type: empty string raises 23502 exception'
);

-- Test 3: Valid value 'Bank' returns itself
select is(
    ores_refdata_validate_party_type_fn(ores_iam_system_tenant_id_fn(), 'Bank'),
    'Bank',
    'party_type: valid value Bank returns itself'
);

-- Test 4: Invalid value raises exception
select throws_ok(
    $$select ores_refdata_validate_party_type_fn(ores_iam_system_tenant_id_fn(), 'INVALID')$$,
    '23503',
    NULL,
    'party_type: invalid value raises 23503 exception'
);

-- =============================================================================
-- Test: ores_refdata_validate_party_status_fn
-- =============================================================================

-- Test 5: NULL raises exception
select throws_ok(
    $$select ores_refdata_validate_party_status_fn(ores_iam_system_tenant_id_fn(), NULL)$$,
    '23502',
    NULL,
    'party_status: NULL raises 23502 exception'
);

-- Test 6: Valid value 'Active' returns itself
select is(
    ores_refdata_validate_party_status_fn(ores_iam_system_tenant_id_fn(), 'Active'),
    'Active',
    'party_status: valid value Active returns itself'
);

-- Test 7: Valid value 'Suspended' returns itself
select is(
    ores_refdata_validate_party_status_fn(ores_iam_system_tenant_id_fn(), 'Suspended'),
    'Suspended',
    'party_status: valid value Suspended returns itself'
);

-- Test 8: Invalid value raises exception
select throws_ok(
    $$select ores_refdata_validate_party_status_fn(ores_iam_system_tenant_id_fn(), 'INVALID')$$,
    '23503',
    NULL,
    'party_status: invalid value raises 23503 exception'
);

-- =============================================================================
-- Test: ores_refdata_validate_party_id_scheme_fn
-- =============================================================================

-- Test 9: NULL raises exception
select throws_ok(
    $$select ores_refdata_validate_party_id_scheme_fn(ores_iam_system_tenant_id_fn(), NULL)$$,
    '23502',
    NULL,
    'party_id_scheme: NULL raises 23502 exception'
);

-- Test 10: Valid value 'LEI' returns itself
select is(
    ores_refdata_validate_party_id_scheme_fn(ores_iam_system_tenant_id_fn(), 'LEI'),
    'LEI',
    'party_id_scheme: valid value LEI returns itself'
);

-- Test 11: Valid value 'BIC' returns itself
select is(
    ores_refdata_validate_party_id_scheme_fn(ores_iam_system_tenant_id_fn(), 'BIC'),
    'BIC',
    'party_id_scheme: valid value BIC returns itself'
);

-- Test 12: Invalid value raises exception
select throws_ok(
    $$select ores_refdata_validate_party_id_scheme_fn(ores_iam_system_tenant_id_fn(), 'INVALID')$$,
    '23503',
    NULL,
    'party_id_scheme: invalid value raises 23503 exception'
);

-- =============================================================================
-- Test: ores_refdata_validate_contact_type_fn
-- =============================================================================

-- Test 13: NULL raises exception
select throws_ok(
    $$select ores_refdata_validate_contact_type_fn(ores_iam_system_tenant_id_fn(), NULL)$$,
    '23502',
    NULL,
    'contact_type: NULL raises 23502 exception'
);

-- Test 14: Valid value 'Legal' returns itself
select is(
    ores_refdata_validate_contact_type_fn(ores_iam_system_tenant_id_fn(), 'Legal'),
    'Legal',
    'contact_type: valid value Legal returns itself'
);

-- Test 15: Valid value 'Settlement' returns itself
select is(
    ores_refdata_validate_contact_type_fn(ores_iam_system_tenant_id_fn(), 'Settlement'),
    'Settlement',
    'contact_type: valid value Settlement returns itself'
);

-- Test 16: Invalid value raises exception
select throws_ok(
    $$select ores_refdata_validate_contact_type_fn(ores_iam_system_tenant_id_fn(), 'INVALID')$$,
    '23503',
    NULL,
    'contact_type: invalid value raises 23503 exception'
);

select * from finish();

rollback;
