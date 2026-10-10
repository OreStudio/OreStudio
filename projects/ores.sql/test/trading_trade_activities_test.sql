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
 * pgTAP tests for the trade activity.
 *
 * Tests cover:
 * - A booking's four rows name one activity
 * - The booking and the state name the activity that booked them
 * - A version naming no activity is refused
 * - A refused second booking writes no activity
 * - An activity is never updated or deleted
 * - A null amend is recorded and versions nothing
 * - An activity names a known activity type
 *
 * Run with: pg_prove -d <database> test/trading_trade_activities_test.sql
 */

begin;

select plan(10);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A recreated database carries the WRLD business centre alone, so the fixture
-- writes the GBLO one this suite names. The suite rolls it back.
insert into ores_refdata_business_centres_tbl (
    code, tenant_id, version, coding_scheme_code, source, description,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'GBLO', ores_utility_system_tenant_id_fn(), 0, 'NONE', 'Internal',
    'London. Trading pgTAP fixture.',
    current_user, current_user, 'system.test', 'Trading pgTAP fixture');

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select username from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username limit 1) as owner_name;

select set_config('app.current_actor', (select owner_name from t_ctx), true);

insert into ores_refdata_portfolios_tbl (id, tenant_id, version, party_id, name,
    parent_portfolio_id, purpose_type, is_virtual, sandbox_id, status,
    modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-0000000ac000', ores_utility_system_tenant_id_fn(), 0,
    party_id, 'ACTTEST-PORTFOLIO', null, 'Risk', false, null, 'Active', owner_name,
    owner_name, 'system.new_record', 'test'
from t_ctx;

insert into ores_refdata_books_tbl (id, tenant_id, version, party_id, name,
    parent_portfolio_id, functional_currency, book_status, regulatory_book_type,
    is_sweepable, rates_centre_code, sandbox_id,
    book_purpose_type, ledger_feed_type,
    modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-0000000ab000', ores_utility_system_tenant_id_fn(), 0,
    party_id, 'ACTTEST-BOOK', '00000000-0000-0000-0000-0000000ac000', 'USD', 'Active',
    'Trading', false, 'GBLO', null, 'Test', 'None',
    owner_name, owner_name, 'system.new_record', 'test'
from t_ctx;

-- A booking writes the four rows the service writes, so this test exercises
-- the schema the service writes against. The service is tested in
-- service_trade_operations_service_tests.cpp.
create or replace function pg_temp.book_trade(p_id uuid)
returns uuid as $$
declare
    v_activity_id uuid := gen_random_uuid();
begin
    insert into ores_trading_trades_tbl (id, tenant_id, party_id, trade_type,
        counterparty_scope, booking_nature, entry_channel,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_id, ores_utility_system_tenant_id_fn(), party_id, 'Swap',
        'intra_entity', 'actual', 'manual',
        current_user, current_user, 'system.new_record', 'Trade pgTAP fixture'
    from t_ctx;

    insert into ores_trading_trade_activities_tbl (id, tenant_id, party_id,
        activity_type_code, actor, occurred_at, comment,
        modified_by, performed_by, change_reason_code, change_commentary)
    select v_activity_id, ores_utility_system_tenant_id_fn(), party_id,
        'new_booking', owner_name, now(), 'booked by test',
        current_user, current_user, 'system.new_record', 'Trade activity pgTAP fixture'
    from t_ctx;

    insert into ores_trading_trade_bookings_tbl (trade_id, trade_activity_id, tenant_id,
        version, book_id, modified_by, performed_by, change_reason_code,
        change_commentary)
    select p_id, v_activity_id, ores_utility_system_tenant_id_fn(), 0,
        '00000000-0000-0000-0000-0000000ab000', owner_name, owner_name,
        'system.new_record', 'booked by test'
    from t_ctx;

    insert into ores_trading_trade_states_tbl (trade_id, trade_activity_id, tenant_id,
        version, status_id, modified_by, performed_by, change_reason_code,
        change_commentary)
    select p_id, v_activity_id, ores_utility_system_tenant_id_fn(), 0,
        ores_utility_nil_uuid_fn(), owner_name, owner_name, 'system.new_record', 'test'
    from t_ctx;

    return v_activity_id;
end;
$$ language plpgsql;

create temp table t_booked on commit drop as
select pg_temp.book_trade('00000000-0000-0000-0000-0000000aa001'::uuid) as activity_id;

select results_eq(
    $$select a.activity_type_code, a.actor = c.owner_name, a.party_id = c.party_id, a.comment
      from ores_trading_trade_activities_tbl a, t_booked b, t_ctx c
      where a.id = b.activity_id$$,
    $$values ('new_booking'::text, true, true, 'booked by test'::text)$$,
    'booking writes the activity it returns, with its type, actor, party and comment');

select results_eq(
    $$select (select trade_activity_id from ores_trading_trade_bookings_tbl
              where trade_id = '00000000-0000-0000-0000-0000000aa001'),
             (select trade_activity_id from ores_trading_trade_states_tbl
              where trade_id = '00000000-0000-0000-0000-0000000aa001')$$,
    $$select activity_id, activity_id from t_booked$$,
    'the booking and the state name the activity that booked them');

select throws_ok(
    $$insert into ores_trading_trade_identifiers_tbl (trade_id, trade_activity_id, id_type,
          tenant_id, version, id_value, modified_by, performed_by,
          change_reason_code, change_commentary)
      select '00000000-0000-0000-0000-0000000aa001', '00000000-0000-0000-0000-0000000ad0ff',
          'UTI', ores_utility_system_tenant_id_fn(), 0, 'UTI-ACTTEST', owner_name,
          owner_name, 'system.new_record', 'test'
      from t_ctx$$,
    '23503',
    null,
    'a version naming no activity is refused');

select throws_ok(
    $$select pg_temp.book_trade('00000000-0000-0000-0000-0000000aa001'::uuid)$$,
    '23505',
    null,
    'a second booking of the same trade is refused');

select is(
    (select count(*) from ores_trading_trade_activities_tbl
     where comment = 'booked by test'),
    1::bigint,
    'the refused booking wrote no activity');

-- No trading entity is immutable any more. A delete closes the current version
-- and keeps the row as history.
select lives_ok(
    $$delete from ores_trading_trade_activities_tbl
      where id = (select activity_id from t_booked)$$,
    'a delete of an activity is accepted');

select is(
    (select count(*)::int from ores_trading_trade_activities_tbl
     where id = (select activity_id from t_booked)
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'the closed activity is no longer current');

create temp table t_versions_before on commit drop as
select (select count(*) from ores_trading_trade_bookings_tbl
        where trade_id = '00000000-0000-0000-0000-0000000aa001') as bookings,
       (select count(*) from ores_trading_trade_states_tbl
        where trade_id = '00000000-0000-0000-0000-0000000aa001') as states;

insert into ores_trading_trade_activities_tbl (id, tenant_id, party_id,
    activity_type_code, actor, occurred_at, comment,
    modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-0000000ad001', ores_utility_system_tenant_id_fn(), party_id,
    'null_amend', owner_name, now(), 'entered the values already held',
    current_user, current_user, 'system.new_record', 'Trade activity pgTAP fixture'
from t_ctx;

select is(
    (select count(*) from ores_trading_trade_activities_tbl
     where id = '00000000-0000-0000-0000-0000000ad001' and activity_type_code = 'null_amend'),
    1::bigint,
    'a null amend is recorded as an activity');

select results_eq(
    $$select (select count(*) from ores_trading_trade_bookings_tbl
              where trade_id = '00000000-0000-0000-0000-0000000aa001'),
             (select count(*) from ores_trading_trade_states_tbl
              where trade_id = '00000000-0000-0000-0000-0000000aa001')$$,
    $$select bookings, states from t_versions_before$$,
    'a null amend versions nothing');

select throws_ok(
    $$insert into ores_trading_trade_activities_tbl (id, tenant_id, party_id,
          activity_type_code, actor, occurred_at, comment,
          modified_by, performed_by, change_reason_code, change_commentary)
      select '00000000-0000-0000-0000-0000000ad002', ores_utility_system_tenant_id_fn(),
          party_id, 'no_such_activity', owner_name, now(), 'test',
          current_user, current_user, 'system.new_record', 'Trade activity pgTAP fixture'
      from t_ctx$$,
    '23503',
    null,
    'an activity names a known activity type');

select * from finish();

rollback;
