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
 * pgTAP tests for the trade links.
 *
 * Tests cover:
 * - A link joins two known trades under a known type
 * - The same link cannot be written twice
 * - The type names a seeded catalogue entry
 * - Both ends name existing trades
 * - The activity that made the link exists
 * - The link carries the party of its from end, pinned to that trade
 * - A link cannot join a trade to itself
 *
 * Run with: pg_prove -d <database> test/trading_trade_links_test.sql
 */

begin;

select plan(8);

-- Row-level security applies to the test user too: state the tenant before
-- writing anything. The tenant comes first because the fixture below writes
-- rows the policies then filter.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A recreated database carries the system party and nothing else, so the
-- fixture writes the second party and the counterparty this suite names. The
-- suite rolls them back.
insert into ores_refdata_parties_tbl (
    id, tenant_id, full_name, short_code, party_category, party_type,
    business_center_code, parent_party_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000cf003'::uuid, ores_utility_system_tenant_id_fn(),
    'Trade Links Other Party', 'TTL-OP', 'Operational', 'Corporate',
    'WRLD', null, 'Active', current_user, current_user,
    'system.test', 'Trade links pgTAP fixture');

insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_counterparty_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000cf103'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'Trade Links Counterparty', 'TTL-CP', 'Corporate',
    null, 'Active', current_user, current_user,
    'system.test', 'Trade links pgTAP fixture');

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TTL-OP'
          and valid_to = ores_utility_infinity_timestamp_fn()) as other_party_id,
       (select id from ores_refdata_counterparties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TTL-CP'
          and valid_to = ores_utility_infinity_timestamp_fn()) as counterparty_id;

-- Two trades, because a link needs both ends. The anchored trade is the one
-- the link starts at; the replacement is where it ends.
insert into ores_trading_trades_tbl (id, tenant_id, party_id, counterparty_id,
    trade_type, counterparty_scope, booking_nature, entry_channel)
select '00000000-0000-0000-0000-0000000ca003', ores_utility_system_tenant_id_fn(), party_id,
    counterparty_id, 'Swap', 'external', 'actual', 'manual'
from t_ctx;

insert into ores_trading_trades_tbl (id, tenant_id, party_id, counterparty_id,
    trade_type, counterparty_scope, booking_nature, entry_channel)
select '00000000-0000-0000-0000-0000000ca004', ores_utility_system_tenant_id_fn(), party_id,
    counterparty_id, 'Swap', 'external', 'actual', 'manual'
from t_ctx;

create or replace function pg_temp.activity(p_party uuid, p_type text default 'new_booking')
returns uuid as $$
    insert into ores_trading_trade_activities_tbl (id, tenant_id, party_id,
        activity_type_code, actor, occurred_at, comment)
    values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), p_party, p_type, 'test',
        now(), 'test')
    returning id;
$$ language sql;

create or replace function pg_temp.link(p_from uuid, p_to uuid, p_type text,
    p_party uuid default null, p_activity uuid default null)
returns void as $$
    insert into ores_trading_trade_links_tbl (from_trade_id, to_trade_id, link_type,
        trade_activity_id, tenant_id, version, party_id, modified_by, performed_by,
        change_reason_code, change_commentary)
    select p_from, p_to, p_type,
        coalesce(p_activity, pg_temp.activity(party_id)),
        ores_utility_system_tenant_id_fn(), 0,
        coalesce(p_party, party_id), current_user, current_user,
        'system.new_record', 'test'
    from t_ctx;
$$ language sql;

-- =============================================================================
-- The link itself
-- =============================================================================

select lives_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000ca003',
        '00000000-0000-0000-0000-0000000ca004', 'Roll')$$,
    'a roll joins two known trades');

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000ca003',
        '00000000-0000-0000-0000-0000000ca004', 'Roll')$$,
    '23505', null,
    'the same link twice is refused');

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000ca003',
        '00000000-0000-0000-0000-0000000ca004', 'Nonsense')$$,
    '23503', null,
    'an unknown link type is refused');

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000ca0ff',
        '00000000-0000-0000-0000-0000000ca004', 'Roll')$$,
    '23503', null,
    'a link from an unknown trade is refused');

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000ca003',
        '00000000-0000-0000-0000-0000000ca0ff', 'Roll')$$,
    '23503', null,
    'a link to an unknown trade is refused');

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000ca003',
        '00000000-0000-0000-0000-0000000ca004', 'CloseOut', null,
        '00000000-0000-0000-0000-0000000caf0f')$$,
    '23503', null,
    'a link made by an unknown activity is refused');

-- =============================================================================
-- The copied party and the two ends
-- =============================================================================

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000ca003',
        '00000000-0000-0000-0000-0000000ca004', 'Novation',
        (select other_party_id from t_ctx))$$,
    '23503', null,
    'a link carries the party of its from end, not another party''s');

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000ca003',
        '00000000-0000-0000-0000-0000000ca003', 'Roll')$$,
    '23514', null,
    'a link cannot join a trade to itself');

select * from finish();

rollback;
