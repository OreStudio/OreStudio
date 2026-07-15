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
 * Asset Class Codes Population Script
 *
 * Populates the top-level asset class classification codes, mirroring
 * ores::marketdata::domain::asset_class.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Asset Class Codes ---'

insert into ores_refdata_asset_class_codes_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'fx', 0, 'FX',
     'Foreign exchange: instruments whose value derives from the exchange rate between two currencies. Covers FX spot, forwards, swaps, and vanilla/exotic options, along with the market data that prices them -- spot rates, forward points, and volatility surfaces quoted by currency pair. FX is unique among asset classes in that every trade inherently involves two currencies rather than one underlying, which is why FX risk shows up as a cross-cutting concern in every other asset class (a USD-denominated equity option still carries FX risk for a EUR-based book).',
     1, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'rates', 0, 'Rates',
     'Interest rate products: instruments whose value derives from the level or shape of a yield curve for a given currency and index (e.g. USD SOFR, EUR ESTR). Covers deposits, forward rate agreements, interest rate swaps and swaptions, caps/floors, and cross-currency swaps, together with the curve-construction market data -- discount factors, zero rates, and forward rates bootstrapped from quoted instruments at standard tenors. Rates is typically the first curve built in any pricing pipeline, since discounting and forward-rate projection for every other asset class ultimately depends on it.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'credit', 0, 'Credit',
     'Credit risk products: instruments whose value derives from the likelihood and severity of an issuer or reference entity defaulting on its obligations. Covers credit default swaps (single-name and index), credit-linked notes, and the underlying market data -- CDS spreads, hazard rate curves bootstrapped from those spreads, and assumed recovery rates. Credit curves are quoted per reference entity (or index series) and are combined with a rates curve to produce risky discount factors for defaultable cashflows.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'equity', 0, 'Equity',
     'Equity products: instruments whose value derives from the price of a single stock, a basket of stocks, or an equity index. Covers equity spot and forward positions, vanilla and exotic equity options (including barrier, Asian, and variance-swap structures), and the associated market data -- spot prices, dividend yield curves, and implied volatility surfaces quoted by strike and expiry. Equity derivatives pricing typically layers a dividend/borrow-cost forward curve and a volatility surface on top of the rates discount curve.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'commodity', 0, 'Commodity',
     'Commodity products: instruments whose value derives from the price of a physical commodity (energy, metals, agriculturals) or a commodity index/future. Covers commodity forwards, swaps, and options (including Asian and spread structures), and the associated market data -- forward curves (which, unlike FX or rates, are often in backwardation or contango driven by storage cost and convenience yield rather than pure interest-rate parity) and volatility surfaces per delivery point/contract month.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'inflation', 0, 'Inflation',
     'Inflation-linked products: instruments whose value derives from a published price index (e.g. US CPI, UK RPI/CPI, EU HICP). Covers zero-coupon and year-on-year inflation swaps, inflation caps/floors, and inflation-linked bonds, together with the associated market data -- the inflation fixing/index curve (historical and forward-projected, with a seasonality adjustment for the index''s known within-year pattern) and inflation volatility surfaces. Inflation curves are built and discounted against the nominal rates curve for the same currency.',
     6, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'bond', 0, 'Bond',
     'Bond products: fixed-income securities that pay a defined schedule of coupons and a principal redemption, whose value derives from a discount curve plus an issuer-specific credit/liquidity spread. Covers vanilla fixed and floating-rate bonds, callable/convertible bonds, and bond futures/options, together with the associated market data -- clean/dirty prices, yield-to-maturity, and issuer spread curves quoted over the risk-free (or asset-swap) curve. Government bonds are also a primary source instrument for bootstrapping the risk-free rates curve itself.',
     7, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'cross_asset', 0, 'Cross Asset',
     'Cross-asset products: instruments and risk measures that genuinely span more than one of the other asset classes rather than sitting cleanly within a single one -- for example, a hybrid note whose payoff depends jointly on an equity index and an FX rate, or a correlation/quanto adjustment that couples an equity or commodity underlying denominated in a foreign currency back to the book''s base currency. This category exists for the small set of instruments and market data (cross-asset correlations, quanto adjustments) that a single-asset-class tag would misrepresent, not as a catch-all default.',
     8, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_asset_class_codes' as entity, count(*) as count
from ores_refdata_asset_class_codes_tbl;
