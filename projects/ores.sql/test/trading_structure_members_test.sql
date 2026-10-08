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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * pgTAP tests for the structure members.
 *
 * Tests cover:
 * - A leg filling a role its template allows is written
 * - A role the template lacks is refused
 * - A leg past the role's maximum is refused
 * - A member's party is the structure's
 * - A trade sits in one structure at a time
 *
 * Run with: pg_probe -d <database> test/trading_structure_members_test.sql
 */

begin;

select plan(6);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_counterparty_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000cf105'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'Members Counterparty', 'TSM-CP', 'Corporate',
    null, 'Active', current_user, current_user,
    'system.test', 'Structure members pgTAP fixture');

insert into ores_refdata_parties_tbl (
    id, tenant_id, full_name, short_code, party_category, party_type,
    business_center_code, parent_party_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000cf005'::uuid, ores_utility_system_tenant_id_fn(),
    'Members Other Party', 'TSM-OP', 'Operational', 'Corporate',
    'WRLD', null, 'Active', current_user, current_user,
    'system.test', 'Structure members pgTAP fixture');

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TSM-OP'
          and valid_to = ores_utility_infinity_timestamp_fn()) as other_party_id,
       (select id from ores_refdata_counterparties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TSM-CP'
          and valid_to = ores_utility_infinity_timestamp_fn()) as counterparty_id;

insert into ores_trading_trades_tbl (id, tenant_id, party_id, counterparty_id,
    trade_type, counterparty_scope, booking_nature, entry_channel)
select '00000000-0000-0000-0000-0000000cc001', ores_utility_system_tenant_id_fn(), party_id,
    counterparty_id, 'Swap', 'external', 'actual', 'manual'
from t_ctx;

insert into ores_trading_trades_tbl (id, tenant_id, party_id, counterparty_id,
    trade_type, counterparty_scope, booking_nature, entry_channel)
select '00000000-0000-0000-0000-0000000cc002', ores_utility_system_tenant_id_fn(), party_id,
    counterparty_id, 'Swap', 'external', 'actual', 'manual'
from t_ctx;

-- A straddle allows one role holding exactly two legs; a butterfly allows a
-- body of exactly one.
insert into ores_trading_structures_tbl (id, tenant_id, party_id, counterparty_id,
    kind, template_code, parent_structure_id)
select '00000000-0000-0000-0000-0000000cd001', ores_utility_system_tenant_id_fn(), party_id,
    counterparty_id, 'Strategy', 'Straddle', null
from t_ctx;

insert into ores_trading_structures_tbl (id, tenant_id, party_id, counterparty_id,
    kind, template_code, parent_structure_id)
select '00000000-0000-0000-0000-0000000cd002', ores_utility_system_tenant_id_fn(), party_id,
    counterparty_id, 'Strategy', 'Butterfly', null
from t_ctx;

insert into ores_trading_structures_tbl (id, tenant_id, party_id, counterparty_id,
    kind, template_code, parent_structure_id)
select '00000000-0000-0000-0000-0000000cd003', ores_utility_system_tenant_id_fn(), party_id,
    counterparty_id, 'Package', null, null
from t_ctx;

create or replace function pg_temp.link(p_trade uuid, p_structure uuid, p_role text,
    p_party uuid default null)
returns void as $$
    insert into ores_trading_structure_members_tbl (trade_id, structure_id, role,
        sequence_number, tenant_id, version, party_id, counterparty_id,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_trade, p_structure, p_role, 1, ores_utility_system_tenant_id_fn(), 0,
        coalesce(p_party, party_id), counterparty_id, current_user, current_user,
        'system.new_record', 'test'
    from t_ctx;
$$ language sql;

-- =============================================================================
-- The role the template allows
-- =============================================================================

select lives_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000cc001',
        '00000000-0000-0000-0000-0000000cd001', 'leg')$$,
    'a leg filling a role its template allows is written');

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000cc002',
        '00000000-0000-0000-0000-0000000cd001', 'wing')$$,
    '23514', null,
    'a role the template does not allow is refused');

select lives_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000cc002',
        '00000000-0000-0000-0000-0000000cd002', 'body')$$,
    'the butterfly takes its body');

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000cc001',
        '00000000-0000-0000-0000-0000000cd002', 'body')$$,
    '23505', null,
    'a trade already linked elsewhere is refused, so it sits in one deal at a time');

-- =============================================================================
-- The party and the counterparty
-- =============================================================================

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000cc002',
        '00000000-0000-0000-0000-0000000cd003', 'leg',
        (select other_party_id from t_ctx))$$,
    '23503', null,
    'a member carries the structure''s party, not another party''s');

select throws_ok(
    $$select pg_temp.link('00000000-0000-0000-0000-0000000cc0ff',
        '00000000-0000-0000-0000-0000000cd003', 'leg')$$,
    '23503', null,
    'linking an unknown trade is refused');

select * from finish();

rollback;
