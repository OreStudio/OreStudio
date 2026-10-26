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
 * Convention Kinds Population Script
 *
 * Seeds the database with the coded catalogue of the convention kinds: the 26
 * ores.refdata models whose #+type: is ores.codegen.entity and whose name ends
 * in _convention. Each code is the model's name with the word "convention"
 * removed, upper-cased: the kind of =tenor_basis_swap_convention= is
 * =TENOR_BASIS_SWAP=. A central =convention= row names its kind with one of
 * these codes.
 *
 * The three other files the directory lists under "convention" are not kinds
 * and are not seeded: =business_day_convention_type= is a coded lookup, and
 * =currency_pair_convention_calendar= and =tenor_convention_resolution= are
 * junctions.
 *
 * Each kind carries the asset class of the product model it serves, which a
 * screen groups the kinds by, and the element that writes it in ORE's
 * conventions document, which ties its terms to the ORE parameter contract.
 * TENOR has no element: the document does not carry it.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE so that
 * name/description text is kept up-to-date in a single atomic statement.
 */

\echo '--- Convention Kinds ---'

insert into ores_refdata_convention_kinds_tbl (
    tenant_id, code, version, name, description, asset_class, ore_element, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'AVERAGE_OIS', 0, 'Average OIS',
     'Average overnight indexed swap convention',
     'interest-rates', 'AverageOIS',
     1, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'BMA_BASIS_SWAP', 0, 'BMA Basis Swap',
     'BMA basis swap convention',
     'interest-rates', 'BMABasisSwap',
     2, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'BOND_YIELD', 0, 'Bond Yield',
     'Bond yield convention',
     'bond', 'BondYield',
     3, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'CDS', 0, 'CDS',
     'Credit default swap convention',
     'credit', 'CDS',
     4, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'CMS_SPREAD_OPTION', 0, 'CMS Spread Option',
     'Constant maturity swap spread option convention',
     'interest-rates', 'CmsSpreadOption',
     5, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'COMMODITY_FORWARD', 0, 'Commodity Forward',
     'Commodity forward convention',
     'commodity', 'CommodityForward',
     6, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'COMMODITY_FUTURE', 0, 'Commodity Future',
     'Commodity future convention',
     'commodity', 'CommodityFuture',
     7, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'CROSS_CURRENCY_BASIS', 0, 'Cross Currency Basis',
     'Cross currency basis swap convention',
     'fx', 'CrossCurrencyBasis',
     8, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'CROSS_CURRENCY_FIX_FLOAT', 0, 'Cross Currency Fix Float',
     'Cross currency fixed versus floating swap convention',
     'fx', 'CrossCurrencyFixFloat',
     9, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'CURRENCY_PAIR', 0, 'Currency Pair',
     'Currency pair convention',
     'fx', 'FX',
     10, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'DEPOSIT', 0, 'Deposit',
     'Deposit convention',
     'interest-rates', 'Deposit',
     11, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'FRA', 0, 'FRA',
     'Forward rate agreement convention',
     'interest-rates', 'FRA',
     12, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'FUTURE', 0, 'Future',
     'Interest rate future convention',
     'interest-rates', 'Future',
     13, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'FX_OPTION', 0, 'FX Option',
     'Foreign exchange option convention',
     'fx', 'FxOption',
     14, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'IBOR_INDEX', 0, 'Ibor Index',
     'Interbank offered rate index convention',
     'interest-rates', 'IborIndex',
     15, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'INFLATION_SWAP', 0, 'Inflation Swap',
     'Inflation swap convention',
     'inflation', 'InflationSwap',
     16, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'INTRADAY_POWER_LOAD', 0, 'Intraday Power Load',
     'Intraday power load convention',
     'commodity', 'IntradayPowerLoad',
     17, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'OIS', 0, 'OIS',
     'Overnight indexed swap convention',
     'interest-rates', 'OIS',
     18, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'OVERNIGHT_INDEX', 0, 'Overnight Index',
     'Overnight index convention',
     'interest-rates', 'OvernightIndex',
     19, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'SWAP', 0, 'Swap',
     'Vanilla interest rate swap convention',
     'interest-rates', 'Swap',
     20, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'SWAP_INDEX', 0, 'Swap Index',
     'Swap index convention',
     'interest-rates', 'SwapIndex',
     21, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'TENOR', 0, 'Tenor',
     'Tenor convention',
     'interest-rates', null,
     22, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'TENOR_BASIS_SWAP', 0, 'Tenor Basis Swap',
     'Tenor basis swap convention, where each leg pays a different tenor off its own index',
     'interest-rates', 'TenorBasisSwap',
     23, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'TENOR_BASIS_TWO_SWAP', 0, 'Tenor Basis Two Swap',
     'Two-tenor basis swap convention',
     'interest-rates', 'TenorBasisTwoSwap',
     24, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'ZERO', 0, 'Zero',
     'Zero coupon convention',
     'interest-rates', 'Zero',
     25, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds'),
    (ores_utility_system_tenant_id_fn(), 'ZERO_INFLATION_INDEX', 0, 'Zero Inflation Index',
     'Zero coupon inflation index convention',
     'inflation', 'ZeroInflationIndex',
     26, current_user, current_user, 'system.initial_load', 'Initial population of convention kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    name = excluded.name,
    description = excluded.description,
    asset_class = excluded.asset_class,
    ore_element = excluded.ore_element,
    display_order = excluded.display_order,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_convention_kinds' as entity, count(*) as count
from ores_refdata_convention_kinds_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
