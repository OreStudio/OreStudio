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
 * pgTAP tests for ores_trading_identifiers_tbl trigger behavior.
 *
 * Tests cover:
 * - Valid insert gets version 1
 * - trade_id soft FK validation rejects invalid UUIDs
 * - id_type validation rejects invalid codes
 * - issuing_party_id validates against both parties and counterparties tables
 * - issuing_party_id = null is accepted
 *
 * Run with: pg_prove -d <database> test/trade_identifiers_test.sql
 */

begin;

select plan(7);

-- =============================================================================
-- Setup: prerequisites
-- =============================================================================

insert into ores_refdata_books_tbl (
    id, tenant_id, version, name, description, party_id,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'd0000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'IDENT-TEST-BOOK', 'Test book for identifier tests',
    'd0000000-0000-0000-0000-000000000010'::uuid,
    current_user, current_user, 'system.test', 'Test book'
);

insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, description,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'd0000000-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'd0000000-0000-0000-0000-000000000010'::uuid,
    'IDENT-TEST-PORTFOLIO', 'Test portfolio for identifier tests',
    current_user, current_user, 'system.test', 'Test portfolio'
);

insert into ores_trading_trades_tbl (
    id, tenant_id, version,
    book_id, portfolio_id,
    trade_type, netting_set_id, lifecycle_event,
    trade_date, execution_timestamp, effective_date, termination_date,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'd1000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'd0000000-0000-0000-0000-000000000001'::uuid,
    'd0000000-0000-0000-0000-000000000002'::uuid,
    'Swap', 'NS-IDENT-001', 'New',
    current_date, current_timestamp, current_date, current_date + interval '1 year',
    current_user, current_user, 'system.test', 'Trade for identifier tests'
);

-- A counterparty to use as issuing_party_id
insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version,
    full_name, short_code, party_type, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'd2000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'Issuer Corp', 'ISSCORP', 'Corporate', 'GBLO', 'Active',
    current_user, current_user, 'system.test', 'Test issuer'
);

-- =============================================================================
-- Test 1: Valid insert with null issuing_party_id gets version 1
-- =============================================================================

insert into ores_trading_identifiers_tbl (
    id, tenant_id, version,
    trade_id, issuing_party_id, id_value, id_type, id_scheme,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'd3000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'd1000000-0000-0000-0000-000000000001'::uuid, null,
    'UTI-TEST-001', 'UTI', null,
    current_user, current_user, 'system.test', 'UTI test insert'
);

select is(
    (select version from ores_trading_identifiers_tbl
     where id = 'd3000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'identifier insert: first version is 1'
);

-- =============================================================================
-- Test 2: trade_id soft FK rejects invalid UUID
-- =============================================================================

select throws_ok(
    $$insert into ores_trading_identifiers_tbl (
        id, tenant_id, version,
        trade_id, issuing_party_id, id_value, id_type, id_scheme,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'd3000000-0000-0000-0000-000000000099'::uuid,
        ores_iam_system_tenant_id_fn(), 0,
        'deadbeef-dead-dead-dead-deaddeadbeef'::uuid, null,
        'UTI-BAD', 'UTI', null,
        current_user, current_user, 'system.test', 'Bad trade_id'
    )$$,
    '23503',
    NULL,
    'identifier insert: invalid trade_id raises 23503'
);

-- =============================================================================
-- Test 3: id_type validation rejects invalid codes
-- =============================================================================

select throws_ok(
    $$insert into ores_trading_identifiers_tbl (
        id, tenant_id, version,
        trade_id, issuing_party_id, id_value, id_type, id_scheme,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'd3000000-0000-0000-0000-000000000098'::uuid,
        ores_iam_system_tenant_id_fn(), 0,
        'd1000000-0000-0000-0000-000000000001'::uuid, null,
        'BAD-VALUE', 'INVALID_TYPE', null,
        current_user, current_user, 'system.test', 'Bad id_type'
    )$$,
    '23503',
    NULL,
    'identifier insert: invalid id_type raises 23503'
);

-- =============================================================================
-- Test 4: issuing_party_id validates against counterparties table
-- =============================================================================

insert into ores_trading_identifiers_tbl (
    id, tenant_id, version,
    trade_id, issuing_party_id, id_value, id_type, id_scheme,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'd3000000-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'd1000000-0000-0000-0000-000000000001'::uuid,
    'd2000000-0000-0000-0000-000000000001'::uuid,
    'USI-TEST-001', 'USI', null,
    current_user, current_user, 'system.test', 'USI with issuing party'
);

select is(
    (select issuing_party_id::text from ores_trading_identifiers_tbl
     where id = 'd3000000-0000-0000-0000-000000000002'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'd2000000-0000-0000-0000-000000000001',
    'identifier insert: issuing_party_id from counterparties accepted'
);

-- =============================================================================
-- Test 5: issuing_party_id rejects UUID not in parties or counterparties
-- =============================================================================

select throws_ok(
    $$insert into ores_trading_identifiers_tbl (
        id, tenant_id, version,
        trade_id, issuing_party_id, id_value, id_type, id_scheme,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'd3000000-0000-0000-0000-000000000097'::uuid,
        ores_iam_system_tenant_id_fn(), 0,
        'd1000000-0000-0000-0000-000000000001'::uuid,
        'baddbeef-dead-dead-dead-deaddeadbeef'::uuid,
        'USI-BAD', 'USI', null,
        current_user, current_user, 'system.test', 'Bad issuing_party_id'
    )$$,
    '23503',
    NULL,
    'identifier insert: unknown issuing_party_id raises 23503'
);

-- =============================================================================
-- Test 6: Soft delete
-- =============================================================================

delete from ores_trading_identifiers_tbl
where id = 'd3000000-0000-0000-0000-000000000001'::uuid
  and tenant_id = ores_iam_system_tenant_id_fn();

select is(
    (select count(*)::integer from ores_trading_identifiers_tbl
     where id = 'd3000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'identifier delete: record no longer current after soft delete'
);

-- =============================================================================
-- Test 7: Historical record preserved after soft delete
-- =============================================================================

select ok(
    (select count(*) > 0 from ores_trading_identifiers_tbl
     where id = 'd3000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to != ores_utility_infinity_timestamp_fn()),
    'identifier delete: historical record preserved after soft delete'
);

select * from finish();

rollback;
