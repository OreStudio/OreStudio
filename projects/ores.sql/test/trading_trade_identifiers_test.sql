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
 * pgTAP tests for the trade identifiers, party roles and additional fields.
 *
 * Tests cover:
 * - Each row references an existing anchor and carries the anchor's party
 * - An identifier names a known scheme, ORE's included, once per trade
 * - A party role names a known role other than the counterparty
 * - An additional field's ordinal starts at one
 *
 * Run with: pg_prove -d <database> test/trading_trade_identifiers_test.sql
 */

begin;

select plan(12);

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
    '00000000-0000-0000-0000-0000000cf002'::uuid, ores_utility_system_tenant_id_fn(),
    'Trading Identifiers Other Party', 'TTI-OP', 'Operational', 'Corporate',
    'WRLD', null, 'Active', current_user, current_user,
    'system.test', 'Trading pgTAP fixture');

insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_counterparty_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000cf102'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'Trading Identifiers Counterparty', 'TTI-CP', 'Corporate',
    null, 'Active', current_user, current_user,
    'system.test', 'Trading pgTAP fixture');

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TTI-OP'
          and valid_to = ores_utility_infinity_timestamp_fn()) as other_party_id,
       (select id from ores_refdata_counterparties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TTI-CP'
          and valid_to = ores_utility_infinity_timestamp_fn()) as counterparty_id;

insert into ores_trading_trades_tbl (id, tenant_id, party_id, counterparty_id,
    trade_type, counterparty_scope, booking_nature, entry_channel,
    modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-0000000ca001', ores_utility_system_tenant_id_fn(), party_id,
    counterparty_id, 'Swap', 'external', 'actual', 'manual',
    current_user, current_user, 'system.new_record', 'Trade pgTAP fixture'
from t_ctx;

create or replace function pg_temp.activity(p_party uuid, p_type text default 'new_booking')
returns uuid as $$
    insert into ores_trading_trade_activities_tbl (id, tenant_id, party_id,
        activity_type_code, actor, occurred_at, comment,
        modified_by, performed_by, change_reason_code, change_commentary)
    values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), p_party, p_type, 'test',
        now(), 'test',
        current_user, current_user, 'system.new_record', 'Trade activity pgTAP fixture')
    returning id;
$$ language sql;

create or replace function pg_temp.identifier(p_trade uuid, p_scheme text, p_value text,
    p_party uuid default null)
returns void as $$
    insert into ores_trading_trade_identifiers_tbl (trade_id, trade_activity_id, id_type,
        tenant_id, version, id_value, modified_by, performed_by, change_reason_code,
        change_commentary)
    select p_trade, pg_temp.activity(coalesce(p_party, party_id)), p_scheme,
        ores_utility_system_tenant_id_fn(), 0, p_value, current_user, current_user,
        'system.new_record', 'test'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.party_role(p_trade uuid, p_role text)
returns void as $$
    insert into ores_trading_party_roles_tbl (trade_id, trade_activity_id, role, tenant_id,
        version, modified_by, performed_by, change_reason_code,
        change_commentary)
    select p_trade, pg_temp.activity(party_id), p_role, ores_utility_system_tenant_id_fn(), 0, current_user, current_user,
        'system.new_record', 'test'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.field(p_trade uuid, p_sequence int,
    p_party uuid default null)
returns void as $$
    insert into ores_trading_trade_additional_fields_tbl (trade_id, trade_activity_id,
        sequence_number, tenant_id, version, name, value, modified_by, performed_by,
        change_reason_code, change_commentary)
    select p_trade, pg_temp.activity(coalesce(p_party, party_id)), p_sequence,
        ores_utility_system_tenant_id_fn(), 0, 'Desk', 'Rates', current_user, current_user,
        'system.new_record', 'test'
    from t_ctx;
$$ language sql;

-- =============================================================================
-- Identifiers
-- =============================================================================

select lives_ok(
    $$select pg_temp.identifier('00000000-0000-0000-0000-0000000ca001', 'ORE', 'TRADE-1')$$,
    'ORE''s Trade/@id is one identifier scheme');

select lives_ok(
    $$select pg_temp.identifier('00000000-0000-0000-0000-0000000ca001', 'UTI', 'UTI-0001')$$,
    'a trade carries one value per scheme');

select throws_ok(
    $$select pg_temp.identifier('00000000-0000-0000-0000-0000000ca001', 'ORE', 'TRADE-2')$$,
    '23505', null,
    'a second value under the same scheme is refused');

select throws_ok(
    $$select pg_temp.identifier('00000000-0000-0000-0000-0000000ca001', 'ISIN', 'X')$$,
    '23503', null,
    'an unknown scheme is refused');

select throws_ok(
    $$select pg_temp.identifier('00000000-0000-0000-0000-0000000ca0ff', 'USI', 'USI-1')$$,
    '23503', null,
    'an identifier of an unknown trade is refused');

-- An identifier used to carry the trade's party and a case here proved the
-- pin. The children stopped copying the party, so there is no copy left to get
-- wrong and nothing for the case to assert.

-- =============================================================================
-- Party roles
-- =============================================================================

select lives_ok(
    $$select pg_temp.party_role('00000000-0000-0000-0000-0000000ca001', 'CalculationAgent')$$,
    'another party''s role is written');

select throws_ok(
    $$select pg_temp.party_role('00000000-0000-0000-0000-0000000ca001', 'Counterparty')$$,
    '23514', null,
    'the counterparty is the anchor''s, not a role');

select throws_ok(
    $$select pg_temp.party_role('00000000-0000-0000-0000-0000000ca001', 'Sponsor')$$,
    '23503', null,
    'an unknown role is refused');

-- A role used to name a counterparty and a case here proved an unknown one
-- was refused. The table stopped carrying the counterparty, so the argument is
-- gone with the column.

select throws_ok(
    $$select pg_temp.party_role('00000000-0000-0000-0000-0000000ca0ff', 'ExecutingBroker')$$,
    '23503', null,
    'a role of an unknown trade is refused');

-- =============================================================================
-- Additional fields
-- =============================================================================

select lives_ok(
    $$select pg_temp.field('00000000-0000-0000-0000-0000000ca001', 1)$$,
    'an additional field is written');

select throws_ok(
    $$select pg_temp.field('00000000-0000-0000-0000-0000000ca001', 0)$$,
    '23514', null,
    'the ordinal starts at one');

-- An additional field used to carry the trade's party, and this case proved
-- the pin. The table stopped carrying the party, so the case is gone with it.

select throws_ok(
    $$select pg_temp.field('00000000-0000-0000-0000-0000000ca0ff', 1)$$,
    '23503', null,
    'a field of an unknown trade is refused');

select * from finish();

rollback;
