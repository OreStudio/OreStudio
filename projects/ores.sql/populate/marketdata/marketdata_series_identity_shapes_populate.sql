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
 * The identity fields each series family requires and forbids, so the writer that
 * projects an identity row from an oresmd URI can refuse a row that misses one or
 * carries one the family does not use.
 *
 * A family is the asset class and the series subclass the taxonomy already names,
 * narrowed by the index family for an index series. The key is that triple plus
 * the field, because a family is not one shape: ir/index_fixing is an IBOR fixing
 * when the index is ibor -- a currency and an index name -- and a swap fixing when
 * the index is swap -- a currency and a tenor.
 *
 * required means a row of the family that leaves the field empty is not an
 * identity; forbidden means the field states something untrue about the family.
 * A field a family does not mention is one the family does not use, and the writer
 * leaves it as the URI parse left it.
 *
 * unit_ccy is forbidden on every rate family and required on every FX one. A rate
 * is a proportion, not a level of one currency in another, and the column says
 * nothing about a fixing, a swap rate or a curve.
 *
 * The rows here cover the families the curve and fixing work creates: the IBOR and
 * swap fixings, the IR rate series, and the FX spot, forward and volatility
 * families that share the mechanism. A family with no row is not stated yet, and
 * the writer warns for it rather than refusing: the warning is the backlog, and a
 * family an owner states is then refused when it is written wrong.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Series Identity Shapes ---'

insert into ores_marketdata_series_identity_shapes_tbl (
    tenant_id, asset_class, series_subclass, index_family, field, requirement, description,
    version, modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- ─── IR: FIXINGS ─────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'ir', 'index_fixing', 'ibor', 'ccy', 'required',
     'A fixing is a rate of one currency, and the currency is part of what names it.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'index_fixing', 'ibor', 'index', 'required',
     'The index family -- ibor, euribor -- is what the fixing is a fixing of.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'index_fixing', 'ibor', 'index_name', 'required',
     'An IBOR fixing is named by its index, and an overnight convention by its own name, so the name is part of its identity.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'index_fixing', 'ibor', 'unit_ccy', 'forbidden',
     'A rate is a proportion, not a level of one currency in another.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'index_fixing', 'swap', 'ccy', 'required',
     'A swap fixing is a rate of one currency.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'index_fixing', 'swap', 'index', 'required',
     'The index family says the fixing is of a swap rather than of an IBOR.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'index_fixing', 'swap', 'tenor', 'required',
     'A swap fixing is named by its tenor: there is one fixing per tenor and no index name.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'index_fixing', 'swap', 'index_name', 'forbidden',
     'A swap fixing has no index name; its tenor is what names it.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'index_fixing', 'swap', 'unit_ccy', 'forbidden',
     'A rate is a proportion, not a level of one currency in another.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),

    -- ─── IR: RATE SERIES ─────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'ir', 'yield', '', 'ccy', 'required',
     'The curve a rate belongs to is a currency, and the rate is of that currency.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'yield', '', 'instrument_type', 'required',
     'A deposit, a swap and a basis swap at the same tenor are different rates.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'yield', '', 'quote_type', 'required',
     'The rate quoted and the spread quoted are two series of one instrument.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'ir', 'yield', '', 'unit_ccy', 'forbidden',
     'A rate is a proportion, not a level of one currency in another; a cross-currency basis states its currencies in the fixed and float fields.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),

    -- ─── FX: FIXINGS ─────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'fx', 'index_fixing', 'fx', 'ccy', 'required',
     'An FX fixing is the price of one currency in another, so both are part of its identity.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'index_fixing', 'fx', 'unit_ccy', 'required',
     'An FX fixing is the price of one currency in another, so both are part of its identity.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'index_fixing', 'fx', 'index', 'required',
     'The index family says the fixing is an FX one and not an interest rate one.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'index_fixing', 'fx', 'source', 'required',
     'An FX fixing is the fixing of one authority; two authorities are two series.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),

    -- ─── FX: SPOT, FORWARD AND VOLATILITY ────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'fx', 'spot', '', 'ccy', 'required',
     'A pair is named by its base currency and its unit currency.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'spot', '', 'unit_ccy', 'required',
     'A pair is named by its base currency and its unit currency.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'spot', '', 'instrument_type', 'required',
     'A spot, a forward and an option of one pair are three instruments.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'spot', '', 'quote_type', 'required',
     'The same pair quoted as a price and as points are two series.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'forward', '', 'ccy', 'required',
     'A forward is named by the pair it is for.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'forward', '', 'unit_ccy', 'required',
     'A forward is named by the pair it is for.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'forward', '', 'instrument_type', 'required',
     'A forward and an option of one pair are two instruments.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'forward', '', 'quote_type', 'required',
     'The same forward quoted as a price and as points are two series.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'volatility', '', 'ccy', 'required',
     'A surface is named by the pair it is for and by its surface id.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'volatility', '', 'unit_ccy', 'required',
     'A surface is named by the pair it is for.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'volatility', '', 'instrument_type', 'required',
     'A surface and a forward of one pair are two instruments.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes'),
    (ores_utility_system_tenant_id_fn(), 'fx', 'volatility', '', 'quote_type', 'required',
     'The same surface quoted as a volatility and as a price are two series.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of series identity shapes')
on conflict (tenant_id, asset_class, series_subclass, index_family, field)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'marketdata_series_identity_shapes' as entity, count(*) as count
from ores_marketdata_series_identity_shapes_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
