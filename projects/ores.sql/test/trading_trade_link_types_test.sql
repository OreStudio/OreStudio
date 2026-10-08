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
 * pgTAP tests for the trade link types.
 *
 * Tests cover:
 * - The seeded vocabulary states the role of each end and the effect
 * - A trade event and an annotation are told apart by the effect
 * - Linked cash is not a trade link, because it does not join two trades
 *
 * Run with: pg_prove -d <database> test/trading_trade_link_types_test.sql
 */

begin;

select plan(5);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp view t_link_types as
select code, from_role, to_role, has_economic_effect
from ores_trading_trade_link_types_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();

select results_eq(
    $$select from_role, to_role, has_economic_effect
      from t_link_types where code = 'CloseOut'$$,
    $$values ('original', 'closing', true)$$,
    'a close-out runs from the original trade to the closing one, and is a trade event');

select results_eq(
    $$select from_role, to_role, has_economic_effect
      from t_link_types where code = 'Wash'$$,
    $$values ('client_facing', 'risk_bearing', false)$$,
    'a wash routes risk and is an annotation');

select is(
    (select count(*) from t_link_types),
    12::bigint,
    'the seeded vocabulary is the twelve trade-to-trade types');

select is(
    (select count(*) from t_link_types where has_economic_effect),
    7::bigint,
    'seven of the twelve are trade events');

select is(
    (select count(*) from t_link_types where code = 'LinkedCash'),
    0::bigint,
    'linked cash is absent, because it does not join two trades');

select * from finish();

rollback;
