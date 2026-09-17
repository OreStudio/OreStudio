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
 * Series Subclass Codes Population Script
 *
 * Populates the fine-grained market series subclass classification codes.
 * This table is the source of truth for the taxonomy; no C++ enum mirrors
 * it.
 *
 * The codes are not partitioned by asset class: most appear under several
 * (spot covers FX spot and equity spot; volatility covers FX options,
 * swaptions and commodity options alike). The pairing a producer emits is
 * declared at the series, not here.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Series Subclass Codes ---'

insert into ores_refdata_series_subclass_codes_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'spot', 0, 'Spot',
     'Price of the underlying for immediate or near-immediate delivery: an FX rate, an equity price, a commodity price. The series has no tenor dimension of its own, so each observation names the single point SPOT. FX spot is the canonical case (FX/RATE/EUR/USD), and it is the series that the forward and option series in the same book ultimately reference.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'forward', 0, 'Forward',
     'Price or rate for delivery on a future date. Covers FX forwards and forward points, equity forwards and dividend curves, and commodity forward curves. Unlike spot, a forward series is a curve: each observation names its tenor in point_id, and the value is only meaningful together with that tenor.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'volatility', 0, 'Volatility',
     'Implied volatility quoted on a curve or surface. Covers FX option surfaces, swaption and cap/floor surfaces, and equity and commodity option surfaces. This is the widest subclass: a surface has two coordinates (expiry and strike) where a curve has one, and the point_id encoding is not shared across the asset classes that use it.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'yield', 0, 'Yield',
     'Yield, zero rate, or discount factor curve. The rates-side workhorse: money-market and deposit curves, zero curves, discount curves, and bootstrapped swap curves all land here. Always curve-shaped, always per currency or per currency and index, with point_id naming the pillar. A published index fixing is not a curve and does not belong here; it is an index_fixing.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'basis', 0, 'Basis',
     'Basis spread curve: the difference between two otherwise comparable curves. Covers basis swaps and BMA/SIFMA swaps. A basis quote is meaningful only relative to the two legs it spans, so the qualifier carries both reference names rather than one currency.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'fra', 0, 'FRA',
     'Forward rate agreement and money-market futures curve: the short end of the rates market, quoted as a rate over a forward period rather than as a discount factor.',
     6, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'xccy', 0, 'Cross-Currency Basis',
     'Cross-currency basis swap curve: the spread paid on one currency leg of a currency swap against that currency''s own floating index. It is the curve that makes cross-currency discounting differ from single-currency discounting.',
     7, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'spread', 0, 'Spread',
     'Credit spread or hazard rate curve per reference entity, and the bond spread quoted over a risk-free or asset-swap curve. Values are quotes in basis points rather than prices.',
     8, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'index_credit', 0, 'Index Credit',
     'Credit default swap index series and index options: a basket of single-name credit risk traded as one instrument.',
     9, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'recovery', 0, 'Recovery Rate',
     'Recovery rate assumed on a reference entity in the event of default, per seniority and currency. Scalar per entity, and an input to the hazard-rate bootstrap rather than a traded quote in its own right.',
     10, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'swap', 0, 'Inflation Swap',
     'Inflation swap curve: zero-coupon or year-on-year, quoted per inflation index. Meaningful only alongside a nominal discounting curve and the index''s own fixing history.',
     11, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'capfloor', 0, 'Cap/Floor',
     'Inflation cap and floor volatility surface: implied volatility on an option on an inflation index, zero-coupon or year-on-year, quoted by strike and expiry. The inflation counterpart of swap; the nominal rates cap/floor and swaption surfaces belong to volatility.',
     12, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'seasonality', 0, 'Seasonality',
     'Seasonality adjustment factor: the within-year multiplicative pattern that a headline annual figure does not show. Covers inflation seasonality, the monthly adjustment applied to a price index, and commodity shape profiles, which spread an annual power or gas forward price across the delivery period''s peak and off-peak hours. A per-period adjustment rather than a traded quote.',
     13, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'price', 0, 'Price',
     'Bond price, clean or dirty, quoted per bond or per benchmark issue. The bond asset class''s primary observed market data, alongside yield-to-maturity.',
     14, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'correlation', 0, 'Correlation',
     'Correlation matrix entry across underlyings, currencies, or credit names. Referenced by multi-asset and cross-currency structures whose payoff depends on the joint behaviour of two or more underlyings. Kept in the taxonomy even though no single asset class owns it, because ORE emits correlation market data and an import carrying it must have a home.',
     15, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'prepayment', 0, 'Prepayment',
     'Conditional prepayment rate: the rate at which borrowers repay a mortgage or callable debt ahead of schedule. Quoted per pool or per instrument, and a primary input to the valuation of mortgage-backed and callable structures, where the timing of the cashflows is uncertain rather than their amount. Unlike recovery, which is a loss given default, prepayment is an assumption about early redemption.',
     16, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'transition_probability', 0, 'Transition Probability',
     'Probability that a rated entity moves between two credit ratings, or defaults, over a stated horizon. Quoted per rating provider and per pair of ratings, and read as a matrix rather than a single curve. It is the ratings-basis alternative to a hazard rate curve: both describe the likelihood of default, but a transition matrix also models migration between the non-default states.',
     17, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes'),
    (ores_utility_system_tenant_id_fn(), 'index_fixing', 0, 'Index Fixing',
     'Published reference rate for one index on one date, such as a money-market fixing or an inflation print. One value per date and no pillars, which is what separates it from yield: an index fixing is observed, not bootstrapped, and a curve is built from a history of them rather than the other way round. The index is named in the qualifier.',
     18, current_user, current_user, 'system.initial_load', 'Initial population of series subclass codes')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_series_subclass_codes' as entity, count(*) as count
from ores_refdata_series_subclass_codes_tbl;
