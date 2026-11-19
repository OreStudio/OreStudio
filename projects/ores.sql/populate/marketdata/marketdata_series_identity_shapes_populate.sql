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
 * Series Identity Shapes
 *
 * The identity shape of each series family the curve and fixing work creates: the
 * IBOR and swap fixings, the IR rate series, and the FX fixing, spot, forward and
 * volatility families. A shape is named by its code -- the asset class, the
 * series subclass and the index family joined by dots -- and says what the family
 * is for. Its entries, in marketdata_series_identity_shape_entries_populate.sql,
 * state the fields it requires and forbids.
 *
 * A family is not one shape per subclass: ir.index_fixing is an IBOR fixing when
 * the index is ibor and a swap fixing when the index is swap, so the index family
 * is part of the code.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Series Identity Shapes ---'

insert into ores_marketdata_series_identity_shapes_tbl (
    tenant_id, code, asset_class, series_subclass, index_family, description,
    version, modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'ir.index_fixing.ibor', 'ir', 'index_fixing', 'ibor',
     'A fixing of an interbank offered rate or an overnight index, such as EURIBOR 3M or ESTR: one rate of one currency, published once a day and named by its index.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir.index_fixing.swap', 'ir', 'index_fixing', 'swap',
     'A fixing of a swap rate, such as the EUR 10Y swap rate: one rate of one currency, named by its tenor because a swap index has no name of its own.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir.yield', 'ir', 'yield', '',
     'A quoted interest rate instrument that a curve is built from, such as a deposit, a swap or a basis swap of one currency at one tenor.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx.index_fixing.fx', 'fx', 'index_fixing', 'fx',
     'An FX fixing that an authority publishes, such as the ECB reference rate: the price of one currency in another, as that authority fixes it.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx.spot', 'fx', 'spot', '',
     'The spot price of a currency pair: the price of the base currency in the unit currency, for delivery at spot.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx.forward', 'fx', 'forward', '',
     'A forward price or forward points of a currency pair at one tenor, which an FX forward curve is built from.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx.volatility', 'fx', 'volatility', '',
     'A volatility quote of a currency pair, which an FX volatility surface is built from.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'marketdata_series_identity_shapes' as entity, count(*) as count
from ores_marketdata_series_identity_shapes_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
