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
 * pgTAP tests for ores_trade_ore_envelope_vw.
 *
 * Tests cover:
 * - View returns expected structure for a booked trade
 * - View resolves counterparty name from party role
 * - After novation, view shows updated counterparty
 * - Trades without a Counterparty role are excluded from the view
 *
 * Run with: pg_prove -d <database> test/trade_ore_envelope_view_test.sql
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
    'f0000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'VIEW-TEST-BOOK', 'Test book for view tests',
    'f0000000-0000-0000-0000-000000000010'::uuid,
    current_user, current_user, 'system.test', 'Test book'
);

insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, description,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'f0000000-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'f0000000-0000-0000-0000-000000000010'::uuid,
    'VIEW-TEST-PORTFOLIO', 'Test portfolio for view tests',
    current_user, current_user, 'system.test', 'Test portfolio'
);

insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version,
    full_name, short_code, party_type, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'f2000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'Alpha Bank', 'ALPHA', 'Corporate', 'GBLO', 'Active',
    current_user, current_user, 'system.test', 'Counterparty A'
);

insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version,
    full_name, short_code, party_type, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'f2000000-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'Beta Bank', 'BETA', 'Corporate', 'GBLO', 'Active',
    current_user, current_user, 'system.test', 'Counterparty B'
);

-- Book the initial trade
insert into ores_trading_trades_tbl (
    id, tenant_id, version,
    book_id, portfolio_id,
    trade_type, netting_set_id, lifecycle_event,
    trade_date, execution_timestamp, effective_date, termination_date,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'f1000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'f0000000-0000-0000-0000-000000000001'::uuid,
    'f0000000-0000-0000-0000-000000000002'::uuid,
    'Swap', 'NS-VIEW-001', 'New',
    current_date, current_timestamp, current_date, current_date + interval '1 year',
    current_user, current_user, 'system.test', 'Trade for view tests'
);

-- Assign Counterparty role to Alpha Bank
insert into ores_trading_party_roles_tbl (
    id, tenant_id, version,
    trade_id, counterparty_id, role,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'f3000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'f1000000-0000-0000-0000-000000000001'::uuid,
    'f2000000-0000-0000-0000-000000000001'::uuid,
    'Counterparty',
    current_user, current_user, 'system.test', 'Initial counterparty'
);

-- =============================================================================
-- Test 1: View returns one row for the booked trade
-- =============================================================================

select is(
    (select count(*)::integer from ores_trade_ore_envelope_vw
     where trade_id = 'f1000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()),
    1,
    'view: returns one row for booked trade'
);

-- =============================================================================
-- Test 2: View resolves counterparty name
-- =============================================================================

select is(
    (select counterparty from ores_trade_ore_envelope_vw
     where trade_id = 'f1000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()),
    'Alpha Bank',
    'view: counterparty resolved to Alpha Bank'
);

-- =============================================================================
-- Test 3: View shows correct trade_type and netting_set_id
-- =============================================================================

select is(
    (select trade_type || '|' || netting_set_id from ores_trade_ore_envelope_vw
     where trade_id = 'f1000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()),
    'Swap|NS-VIEW-001',
    'view: trade_type and netting_set_id correct'
);

-- =============================================================================
-- Test 4: Trade without Counterparty role is excluded from view
-- =============================================================================

-- A trade with no party roles assigned
insert into ores_trading_trades_tbl (
    id, tenant_id, version,
    book_id, portfolio_id,
    trade_type, netting_set_id, lifecycle_event,
    trade_date, execution_timestamp, effective_date, termination_date,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'f1000000-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'f0000000-0000-0000-0000-000000000001'::uuid,
    'f0000000-0000-0000-0000-000000000002'::uuid,
    'Swap', 'NS-VIEW-002', 'New',
    current_date, current_timestamp, current_date, current_date + interval '1 year',
    current_user, current_user, 'system.test', 'Trade with no counterparty role'
);

select is(
    (select count(*)::integer from ores_trade_ore_envelope_vw
     where trade_id = 'f1000000-0000-0000-0000-000000000002'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()),
    0,
    'view: trade without Counterparty role excluded'
);

-- =============================================================================
-- Test 5: After novation, view shows updated counterparty (Beta Bank)
-- =============================================================================

-- Apply novation
insert into ores_trading_trades_tbl (
    id, tenant_id, version,
    book_id, portfolio_id,
    trade_type, netting_set_id, lifecycle_event,
    trade_date, execution_timestamp, effective_date, termination_date,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'f1000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'f0000000-0000-0000-0000-000000000001'::uuid,
    'f0000000-0000-0000-0000-000000000002'::uuid,
    'Swap', 'NS-VIEW-001', 'Novation',
    current_date, current_timestamp, current_date, current_date + interval '1 year',
    current_user, current_user, 'system.amendment', 'Novation event'
);

-- Soft-delete Alpha Bank's Counterparty role
delete from ores_trading_party_roles_tbl
where id = 'f3000000-0000-0000-0000-000000000001'::uuid
  and tenant_id = ores_iam_system_tenant_id_fn();

-- Assign Counterparty role to Beta Bank
insert into ores_trading_party_roles_tbl (
    id, tenant_id, version,
    trade_id, counterparty_id, role,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'f3000000-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'f1000000-0000-0000-0000-000000000001'::uuid,
    'f2000000-0000-0000-0000-000000000002'::uuid,
    'Counterparty',
    current_user, current_user, 'system.amendment', 'Post-novation counterparty'
);

select is(
    (select counterparty from ores_trade_ore_envelope_vw
     where trade_id = 'f1000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()),
    'Beta Bank',
    'view: after novation, counterparty updated to Beta Bank'
);

-- =============================================================================
-- Test 6: lifecycle_event in view reflects novation
-- =============================================================================

select is(
    (select lifecycle_event from ores_trade_ore_envelope_vw
     where trade_id = 'f1000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()),
    'Novation',
    'view: lifecycle_event shows Novation after novation event'
);

select * from finish();

rollback;
