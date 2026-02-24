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
 * pgTAP tests for ores_trading_party_roles_tbl trigger behavior.
 *
 * Tests cover:
 * - Valid insert gets version 1
 * - trade_id soft FK validation rejects invalid UUIDs
 * - counterparty_id soft FK validation rejects invalid UUIDs
 * - role validation rejects invalid codes
 * - Soft delete
 *
 * Run with: pg_prove -d <database> test/trade_party_roles_test.sql
 */

begin;

select plan(6);

-- =============================================================================
-- Setup: prerequisites
-- =============================================================================

insert into ores_refdata_books_tbl (
    id, tenant_id, version, name, description, party_id,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'e0000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'ROLES-TEST-BOOK', 'Test book for party role tests',
    'e0000000-0000-0000-0000-000000000010'::uuid,
    current_user, current_user, 'system.test', 'Test book'
);

insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, description,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'e0000000-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'e0000000-0000-0000-0000-000000000010'::uuid,
    'ROLES-TEST-PORTFOLIO', 'Test portfolio for party role tests',
    current_user, current_user, 'system.test', 'Test portfolio'
);

insert into ores_trading_trades_tbl (
    id, tenant_id, version,
    book_id, portfolio_id,
    trade_type, netting_set_id, lifecycle_event,
    trade_date, execution_timestamp, effective_date, termination_date,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'e1000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'e0000000-0000-0000-0000-000000000001'::uuid,
    'e0000000-0000-0000-0000-000000000002'::uuid,
    'Swap', 'NS-ROLES-001', 'New',
    current_date, current_timestamp, current_date, current_date + interval '1 year',
    current_user, current_user, 'system.test', 'Trade for party role tests'
);

insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version,
    full_name, short_code, party_type, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'e2000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'RoleCorp A', 'ROLA', 'Corporate', 'GBLO', 'Active',
    current_user, current_user, 'system.test', 'Test counterparty for roles'
);

-- =============================================================================
-- Test 1: Valid insert gets version 1
-- =============================================================================

insert into ores_trading_party_roles_tbl (
    id, tenant_id, version,
    trade_id, counterparty_id, role,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'e3000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'e1000000-0000-0000-0000-000000000001'::uuid,
    'e2000000-0000-0000-0000-000000000001'::uuid,
    'Counterparty',
    current_user, current_user, 'system.test', 'Test party role insert'
);

select is(
    (select version from ores_trading_party_roles_tbl
     where id = 'e3000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'party role insert: first version is 1'
);

-- =============================================================================
-- Test 2: trade_id soft FK rejects invalid UUID
-- =============================================================================

select throws_ok(
    $$insert into ores_trading_party_roles_tbl (
        id, tenant_id, version,
        trade_id, counterparty_id, role,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'e3000000-0000-0000-0000-000000000099'::uuid,
        ores_iam_system_tenant_id_fn(), 0,
        'deadbeef-dead-dead-dead-deaddeadbeef'::uuid,
        'e2000000-0000-0000-0000-000000000001'::uuid,
        'Counterparty',
        current_user, current_user, 'system.test', 'Bad trade_id'
    )$$,
    '23503',
    NULL,
    'party role insert: invalid trade_id raises 23503'
);

-- =============================================================================
-- Test 3: counterparty_id soft FK rejects invalid UUID
-- =============================================================================

select throws_ok(
    $$insert into ores_trading_party_roles_tbl (
        id, tenant_id, version,
        trade_id, counterparty_id, role,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'e3000000-0000-0000-0000-000000000098'::uuid,
        ores_iam_system_tenant_id_fn(), 0,
        'e1000000-0000-0000-0000-000000000001'::uuid,
        'deadbeef-dead-dead-dead-deaddeadbeef'::uuid,
        'Counterparty',
        current_user, current_user, 'system.test', 'Bad counterparty_id'
    )$$,
    '23503',
    NULL,
    'party role insert: invalid counterparty_id raises 23503'
);

-- =============================================================================
-- Test 4: role validation rejects invalid codes
-- =============================================================================

select throws_ok(
    $$insert into ores_trading_party_roles_tbl (
        id, tenant_id, version,
        trade_id, counterparty_id, role,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'e3000000-0000-0000-0000-000000000097'::uuid,
        ores_iam_system_tenant_id_fn(), 0,
        'e1000000-0000-0000-0000-000000000001'::uuid,
        'e2000000-0000-0000-0000-000000000001'::uuid,
        'INVALID_ROLE',
        current_user, current_user, 'system.test', 'Bad role'
    )$$,
    '23503',
    NULL,
    'party role insert: invalid role raises 23503'
);

-- =============================================================================
-- Test 5: Soft delete
-- =============================================================================

delete from ores_trading_party_roles_tbl
where id = 'e3000000-0000-0000-0000-000000000001'::uuid
  and tenant_id = ores_iam_system_tenant_id_fn();

select is(
    (select count(*)::integer from ores_trading_party_roles_tbl
     where id = 'e3000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'party role delete: record no longer current after soft delete'
);

-- =============================================================================
-- Test 6: Historical record preserved after soft delete
-- =============================================================================

select ok(
    (select count(*) > 0 from ores_trading_party_roles_tbl
     where id = 'e3000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to != ores_utility_infinity_timestamp_fn()),
    'party role delete: historical record preserved after soft delete'
);

select * from finish();

rollback;
