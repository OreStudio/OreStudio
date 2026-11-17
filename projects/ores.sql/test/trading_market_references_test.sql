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
 * pgTAP tests for the market data a set of trades names.
 *
 * Tests cover:
 * - A leg's index address is a requirement of its trade, named by its column
 * - A leg with no index states no requirement
 * - A trade outside the set states none
 * - A read before the leg was written finds none
 *
 * Run with: pg_prove -d <database> test/trading_market_references_test.sql
 */

begin;

select plan(4);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       now() - interval '1 second' as before_booking;

select set_config('app.current_party_id', (select party_id from t_ctx)::text, true);
select set_config('app.visible_party_ids', '{' || (select party_id from t_ctx)::text || '}', true);

create or replace function pg_temp.book_leg(p_trade uuid, p_activity uuid,
    p_leg_type text, p_index text)
returns void as $$
    insert into ores_trading_trades_tbl (id, tenant_id, party_id, trade_type,
        counterparty_scope, booking_nature, entry_channel, origin,
        external_version, modified_by, performed_by, change_reason_code, change_commentary)
    select p_trade, ores_utility_system_tenant_id_fn(), party_id, 'Swap',
        'intra_entity', 'actual', 'manual', 'api', 0,
        current_user, current_user, 'system.new_record', 'Market references pgTAP fixture'
    from t_ctx;

    insert into ores_trading_trade_activities_tbl (id, tenant_id, party_id,
        activity_type_code, actor, occurred_at, comment,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_activity, ores_utility_system_tenant_id_fn(), party_id,
        'new_booking', current_user, now(), 'booked by test',
        current_user, current_user, 'system.new_record', 'Market references pgTAP fixture'
    from t_ctx;

    insert into ores_trading_swap_legs_tbl (trade_id, tenant_id, version, party_id,
        trade_activity_id, leg_number, leg_type_code, currency, floating_index_code,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_trade, ores_utility_system_tenant_id_fn(), 0, party_id,
        p_activity, 1, p_leg_type, 'GBP', p_index,
        current_user, current_user, 'system.new_record', 'Market references pgTAP fixture'
    from t_ctx;
$$ language sql;

select pg_temp.book_leg('00000000-0000-0000-0000-0000000e1001', '00000000-0000-0000-0000-0000000e2001',
    'Floating', 'oresmd://ir/GBP?type=fixing&index=ibor&name=SONIA');
select pg_temp.book_leg('00000000-0000-0000-0000-0000000e1002', '00000000-0000-0000-0000-0000000e2002',
    'Fixed', null);
select pg_temp.book_leg('00000000-0000-0000-0000-0000000e1003', '00000000-0000-0000-0000-0000000e2003',
    'Floating', 'oresmd://ir/EUR?type=fixing&index=ibor&name=ESTER');

select results_eq(
    $$select trade_id, oresmd_uri, source
      from ores_trading_market_references_fn(ores_utility_system_tenant_id_fn(),
          array['00000000-0000-0000-0000-0000000e1001']::uuid[])$$,
    $$values ('00000000-0000-0000-0000-0000000e1001'::uuid,
              'oresmd://ir/GBP?type=fixing&index=ibor&name=SONIA'::text,
              'ores_trading_swap_legs_tbl.floating_index_code'::text)$$,
    'a floating leg requires the index its address names, from the column that holds it');

select is_empty(
    $$select 1 from ores_trading_market_references_fn(ores_utility_system_tenant_id_fn(),
          array['00000000-0000-0000-0000-0000000e1002']::uuid[])$$,
    'a fixed leg requires no index');

select is(
    (select count(*)::int from ores_trading_market_references_fn(ores_utility_system_tenant_id_fn(),
         array['00000000-0000-0000-0000-0000000e1001', '00000000-0000-0000-0000-0000000e1002']::uuid[])
     where oresmd_uri like '%name=ESTER%'),
    0,
    'a trade outside the set states none of its requirements');

select is_empty(
    format($$select 1 from ores_trading_market_references_fn(ores_utility_system_tenant_id_fn(),
          array['00000000-0000-0000-0000-0000000e1001']::uuid[], %L::timestamptz)$$,
          (select before_booking from t_ctx)),
    'a read before the leg was written finds no requirement');

select * from finish();

rollback;
