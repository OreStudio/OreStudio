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
 * pgTAP tests for ores_trading_trades_tbl trigger behavior.
 *
 * Tests cover:
 * - Valid insert gets version 1 and correct temporal columns
 * - book_id soft FK validation rejects invalid UUIDs
 * - portfolio_id soft FK validation rejects invalid UUIDs
 * - trade_type validation rejects invalid codes
 * - activity_type_code validation rejects invalid codes
 * - Temporal versioning: amendment creates new row, closes old row
 *
 * Run with: pg_prove -d <database> test/trade_trades_test.sql
 */

begin;

select plan(9);

-- =============================================================================
-- Setup: insert prerequisite reference data
-- =============================================================================

-- Book (needed for trade FK)
insert into ores_refdata_books_tbl (
    id, tenant_id, version, name, description, party_id,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'c0000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'TRADE-TEST-BOOK', 'Test book for trade tests',
    'c0000000-0000-0000-0000-000000000010'::uuid,
    current_user, current_user,
    'system.test', 'Test book'
);

-- Portfolio (needed for trade FK)
insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, description,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'c0000000-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'c0000000-0000-0000-0000-000000000010'::uuid,
    'TRADE-TEST-PORTFOLIO', 'Test portfolio for trade tests',
    current_user, current_user,
    'system.test', 'Test portfolio'
);

-- Resolve FSM status IDs for tests
do $$ begin
    if not exists (
        select 1 from ores_dq_fsm_machines_tbl
        where name = 'trade_status'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'trade_status FSM not found. Run dq_fsm populate first.';
    end if;
end $$;

-- =============================================================================
-- Test 1: Valid insert gets version 1
-- =============================================================================

insert into ores_trading_trades_tbl (
    id, tenant_id, version,
    book_id, portfolio_id,
    trade_type, netting_set_id, activity_type_code, status_id,
    trade_date, execution_timestamp, effective_date, termination_date,
    modified_by, performed_by, change_reason_code, change_commentary
) select
    'c1000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'c0000000-0000-0000-0000-000000000001'::uuid,
    'c0000000-0000-0000-0000-000000000002'::uuid,
    'Swap', 'NS-TEST-001', 'new_booking', s.id,
    current_date, current_timestamp, current_date, current_date + interval '1 year',
    current_user, current_user, 'system.test', 'Test trade insert'
from ores_dq_fsm_states_tbl s
join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
where m.name = 'trade_status'
  and m.valid_to = ores_utility_infinity_timestamp_fn()
  and s.name = 'new'
  and s.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select version from ores_trading_trades_tbl
     where id = 'c1000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'trade insert: first version is 1'
);

-- =============================================================================
-- Test 2: valid_to is set to infinity
-- =============================================================================

select is(
    (select valid_to from ores_trading_trades_tbl
     where id = 'c1000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_infinity_timestamp_fn(),
    'trade insert: valid_to set to infinity'
);

-- =============================================================================
-- Test 3: book_id validation rejects invalid UUID
-- =============================================================================

select throws_ok(
    format($$insert into ores_trading_trades_tbl (
        id, tenant_id, version,
        book_id, portfolio_id,
        trade_type, netting_set_id, activity_type_code, status_id,
        trade_date, execution_timestamp, effective_date, termination_date,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'c1000000-0000-0000-0000-000000000099'::uuid,
        ores_iam_system_tenant_id_fn(), 0,
        'deadbeef-dead-dead-dead-deaddeadbeef'::uuid,
        'c0000000-0000-0000-0000-000000000002'::uuid,
        'Swap', 'NS-BAD', 'new_booking', %L::uuid,
        current_date, current_timestamp, current_date, current_date + interval '1 year',
        current_user, current_user, 'system.test', 'Bad book_id'
    )$$,
    (select s.id::text from ores_dq_fsm_states_tbl s
     join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
     where m.name = 'trade_status' and m.valid_to = ores_utility_infinity_timestamp_fn()
       and s.name = 'new' and s.valid_to = ores_utility_infinity_timestamp_fn())
    ),
    '23503',
    NULL,
    'trade insert: invalid book_id raises 23503'
);

-- =============================================================================
-- Test 4: portfolio_id validation rejects invalid UUID
-- =============================================================================

select throws_ok(
    format($$insert into ores_trading_trades_tbl (
        id, tenant_id, version,
        book_id, portfolio_id,
        trade_type, netting_set_id, activity_type_code, status_id,
        trade_date, execution_timestamp, effective_date, termination_date,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'c1000000-0000-0000-0000-000000000098'::uuid,
        ores_iam_system_tenant_id_fn(), 0,
        'c0000000-0000-0000-0000-000000000001'::uuid,
        'deadbeef-dead-dead-dead-deaddeadbeef'::uuid,
        'Swap', 'NS-BAD', 'new_booking', %L::uuid,
        current_date, current_timestamp, current_date, current_date + interval '1 year',
        current_user, current_user, 'system.test', 'Bad portfolio_id'
    )$$,
    (select s.id::text from ores_dq_fsm_states_tbl s
     join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
     where m.name = 'trade_status' and m.valid_to = ores_utility_infinity_timestamp_fn()
       and s.name = 'new' and s.valid_to = ores_utility_infinity_timestamp_fn())
    ),
    '23503',
    NULL,
    'trade insert: invalid portfolio_id raises 23503'
);

-- =============================================================================
-- Test 5: trade_type validation rejects invalid codes
-- =============================================================================

select throws_ok(
    format($$insert into ores_trading_trades_tbl (
        id, tenant_id, version,
        book_id, portfolio_id,
        trade_type, netting_set_id, activity_type_code, status_id,
        trade_date, execution_timestamp, effective_date, termination_date,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'c1000000-0000-0000-0000-000000000097'::uuid,
        ores_iam_system_tenant_id_fn(), 0,
        'c0000000-0000-0000-0000-000000000001'::uuid,
        'c0000000-0000-0000-0000-000000000002'::uuid,
        'INVALID_TYPE', 'NS-BAD', 'new_booking', %L::uuid,
        current_date, current_timestamp, current_date, current_date + interval '1 year',
        current_user, current_user, 'system.test', 'Bad trade_type'
    )$$,
    (select s.id::text from ores_dq_fsm_states_tbl s
     join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
     where m.name = 'trade_status' and m.valid_to = ores_utility_infinity_timestamp_fn()
       and s.name = 'new' and s.valid_to = ores_utility_infinity_timestamp_fn())
    ),
    '23503',
    NULL,
    'trade insert: invalid trade_type raises 23503'
);

-- =============================================================================
-- Test 6: activity_type_code validation rejects invalid codes
-- =============================================================================

select throws_ok(
    format($$insert into ores_trading_trades_tbl (
        id, tenant_id, version,
        book_id, portfolio_id,
        trade_type, netting_set_id, activity_type_code, status_id,
        trade_date, execution_timestamp, effective_date, termination_date,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'c1000000-0000-0000-0000-000000000096'::uuid,
        ores_iam_system_tenant_id_fn(), 0,
        'c0000000-0000-0000-0000-000000000001'::uuid,
        'c0000000-0000-0000-0000-000000000002'::uuid,
        'Swap', 'NS-BAD', 'INVALID_ACTIVITY', %L::uuid,
        current_date, current_timestamp, current_date, current_date + interval '1 year',
        current_user, current_user, 'system.test', 'Bad activity_type_code'
    )$$,
    (select s.id::text from ores_dq_fsm_states_tbl s
     join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
     where m.name = 'trade_status' and m.valid_to = ores_utility_infinity_timestamp_fn()
       and s.name = 'new' and s.valid_to = ores_utility_infinity_timestamp_fn())
    ),
    '23503',
    NULL,
    'trade insert: invalid activity_type_code raises 23503'
);

-- =============================================================================
-- Test 7: Temporal versioning — amendment closes old row, creates new
-- =============================================================================

-- Insert amendment (same id, version 0 = accept current); status stays 'new'
insert into ores_trading_trades_tbl (
    id, tenant_id, version,
    book_id, portfolio_id,
    trade_type, netting_set_id, activity_type_code, status_id,
    trade_date, execution_timestamp, effective_date, termination_date,
    modified_by, performed_by, change_reason_code, change_commentary
) select
    'c1000000-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(), 0,
    'c0000000-0000-0000-0000-000000000001'::uuid,
    'c0000000-0000-0000-0000-000000000002'::uuid,
    'Swap', 'NS-TEST-001', 'amendment', s.id,
    current_date, current_timestamp, current_date, current_date + interval '2 years',
    current_user, current_user, 'system.amendment', 'Test amendment'
from ores_dq_fsm_states_tbl s
join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
where m.name = 'trade_status'
  and m.valid_to = ores_utility_infinity_timestamp_fn()
  and s.name = 'new'
  and s.valid_to = ores_utility_infinity_timestamp_fn();

-- New current row has version 2
select is(
    (select version from ores_trading_trades_tbl
     where id = 'c1000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    2,
    'trade amendment: new version is 2'
);

-- =============================================================================
-- Test 8: Previous row has valid_to set (not infinity)
-- =============================================================================

select is(
    (select count(*)::integer from ores_trading_trades_tbl
     where id = 'c1000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to != ores_utility_infinity_timestamp_fn()),
    1,
    'trade amendment: exactly one historical row exists'
);

-- =============================================================================
-- Test 9: Soft delete via DELETE rule
-- =============================================================================

delete from ores_trading_trades_tbl
where id = 'c1000000-0000-0000-0000-000000000001'::uuid
  and tenant_id = ores_iam_system_tenant_id_fn();

select is(
    (select count(*)::integer from ores_trading_trades_tbl
     where id = 'c1000000-0000-0000-0000-000000000001'::uuid
       and tenant_id = ores_iam_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'trade delete: record no longer current after soft delete'
);

select * from finish();

rollback;
