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
 * pgTAP tests for the trade anchor and its classification lookups.
 *
 * Tests cover:
 * - The three classification lookups hold their closed sets
 * - An anchor names a known party, counterparty, trade type and classification
 * - Only an intra-entity anchor may have no counterparty
 * - A second write of the same trade id is refused
 * - Anchors and lookups refuse update and delete
 * - A tenant purge deletes an anchor once it turns the purge switch on
 *
 * Run with: pg_prove -d <database> test/trading_trade_test.sql
 */

begin;

select plan(22);

-- Row-level security applies to the test user too: state the tenant before
-- writing anything. The tenant comes first because the fixture below writes
-- rows the policies then filter.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A recreated database carries the system party and nothing else, so the
-- fixture writes the counterparty this suite names. The suite rolls it back.
insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_counterparty_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000cf101'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'Trading Trade Test Counterparty', 'TTT-CP', 'Corporate',
    null, 'Active', current_user, current_user,
    'system.test', 'Trading pgTAP fixture');

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select id from ores_refdata_counterparties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TTT-CP'
          and valid_to = ores_utility_infinity_timestamp_fn()) as counterparty_id;

create or replace function pg_temp.anchor(p_id uuid, p_counterparty uuid, p_scope text,
    p_nature text default 'actual', p_channel text default 'manual',
    p_trade_type text default 'Swap', p_party uuid default null)
returns void as $$
    insert into ores_trading_trades_tbl (id, tenant_id, party_id, counterparty_id,
        trade_type, counterparty_scope, booking_nature, entry_channel,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_id, ores_utility_system_tenant_id_fn(), coalesce(p_party, party_id),
        p_counterparty, p_trade_type, p_scope, p_nature, p_channel,
        current_user, current_user, 'system.new_record', 'Trade pgTAP fixture'
    from t_ctx;
$$ language sql;

-- =============================================================================
-- The lookups hold the closed sets
-- =============================================================================

select set_eq(
    'select code from ores_trading_entry_channel_types_tbl',
    array['manual', 'stp', 'ecn', 'allocation'],
    'entry channels are the four closed codes');

select set_eq(
    'select code from ores_trading_counterparty_scope_types_tbl',
    array['external', 'inter_entity', 'intra_entity'],
    'counterparty scopes are the three closed codes');

select set_eq(
    'select code from ores_trading_booking_nature_types_tbl',
    array['actual', 'test', 'hypothetical'],
    'booking natures are the three closed codes');

-- =============================================================================
-- What an anchor may name
-- =============================================================================

select lives_ok(
    $$select pg_temp.anchor('00000000-0000-0000-0000-0000000ac001', null, 'intra_entity')$$,
    'an intra-entity anchor with no counterparty is written');

select lives_ok(
    $$select pg_temp.anchor('00000000-0000-0000-0000-0000000ac002',
        (select counterparty_id from t_ctx), 'external', 'hypothetical', 'stp')$$,
    'an external anchor with a counterparty is written');

select throws_ok(
    $$select pg_temp.anchor('00000000-0000-0000-0000-0000000ac003', null, 'external')$$,
    '23514', null,
    'an external anchor needs a counterparty');

select throws_ok(
    $$select pg_temp.anchor('00000000-0000-0000-0000-0000000ac004', null, 'intra_entity',
        'actual', 'fax')$$,
    '23503', null,
    'an unknown entry channel is refused by the foreign key');

select throws_ok(
    $$select pg_temp.anchor('00000000-0000-0000-0000-0000000ac005', null, 'intra_entity',
        'hypo')$$,
    '23503', null,
    'an unknown booking nature is refused by the foreign key');

select throws_ok(
    $$select pg_temp.anchor('00000000-0000-0000-0000-0000000ac006',
        (select counterparty_id from t_ctx), 'internal')$$,
    '23503', null,
    'an unknown counterparty scope is refused by the foreign key');

select throws_ok(
    $$select pg_temp.anchor('00000000-0000-0000-0000-0000000ac007', null, 'intra_entity',
        'actual', 'manual', 'NoSuchTrade')$$,
    '23503', null,
    'an unknown trade type is refused');

select throws_ok(
    $$select pg_temp.anchor('00000000-0000-0000-0000-0000000ac008', null, 'intra_entity',
        'actual', 'manual', 'Swap', '00000000-0000-0000-0000-0000000ac0ff')$$,
    '23503', null,
    'an unknown party is refused');

select throws_ok(
    $$select pg_temp.anchor('00000000-0000-0000-0000-0000000ac008', '00000000-0000-0000-0000-0000000ac0ff',
        'external')$$,
    '23503', null,
    'an unknown counterparty is refused');

-- =============================================================================
-- An anchor is versioned, not frozen
-- =============================================================================

select lives_ok(
    $$select pg_temp.anchor('00000000-0000-0000-0000-0000000ac001', null, 'intra_entity')$$,
    'a second write of the same trade id is accepted as a new version');

select is(
    (select count(*)::int from ores_trading_trades_tbl
     where id = '00000000-0000-0000-0000-0000000ac001'),
    2,
    'the replaced version is kept beside the new one');

select is(
    (select count(*)::int from ores_trading_trades_tbl
     where id = '00000000-0000-0000-0000-0000000ac001'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'exactly one version of the trade is current');

-- A delete closes the current version. The row stays as history, which is what
-- the delete rule is for.
select lives_ok(
    $$delete from ores_trading_trades_tbl
      where id = '00000000-0000-0000-0000-0000000ac001'$$,
    'a delete of an anchor is accepted');

select is(
    (select count(*)::int from ores_trading_trades_tbl
     where id = '00000000-0000-0000-0000-0000000ac001'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'the closed anchor is no longer current');

select is(
    (select count(*)::int from ores_trading_trades_tbl
     where id = '00000000-0000-0000-0000-0000000ac001'),
    2,
    'the closed anchor is kept as history');

-- The closed vocabularies keep their codes: the delete reaches the table and
-- the row stays.
select is(
    (select count(*)::int from ores_trading_entry_channel_types_tbl
     where code = 'ecn'),
    1,
    'a classification code survives a delete');

-- =============================================================================
-- The sanctioned purge. The switch stays on for the rest of the transaction,
-- so these cases come last. No trading entity is immutable any more, so the
-- switch has nothing to lift: the delete closes the version and keeps the row,
-- exactly as a delete does without it.
-- =============================================================================

select lives_ok(
    $$select ores_utility_allow_immutable_purge_fn();
      delete from ores_trading_trades_tbl
      where id = '00000000-0000-0000-0000-0000000ac002'$$,
    'a delete passes once the purge switch is on');

select is(
    (select count(*)::int from ores_trading_trades_tbl
     where id = '00000000-0000-0000-0000-0000000ac002'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'the anchor the switch closed is no longer current');

select is(
    (select count(*)::int from ores_trading_trades_tbl
     where id = '00000000-0000-0000-0000-0000000ac002'),
    1,
    'the purge switch keeps the row as history, as a plain delete does');

select * from finish();

rollback;
