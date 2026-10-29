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
 * Market Data Series Classification Rules Population Script
 *
 * The taxonomy of a market series, one row per oresmd identity the runtime
 * creates a series for. The ingest reads this table once, builds a classifier
 * from it, and asks that classifier for the asset classes and the series
 * subclass of every series it creates. An identity a corpus adds later, or one
 * a user brings, is a row here rather than a code change.
 *
 * The key is the oresmd identity and not an ORE key. key_kind says which kind of
 * identity a row is keyed by: datum rows by the instrument and quote the URI
 * writes, index rows by the index family, which is what a fixing series is. A
 * quote of the empty string applies to every quote of its instrument, because
 * most instruments classify the same way whatever their quote carries.
 *
 * Two rows do not fit the literal shape, and the columns say so: CORRELATION
 * names no class and reads the two from its datum's operands, and the index row
 * reads the class from the index family's own schema row.
 *
 * There is no row for ORE's GENERIC-MD wrapper. It is not a market datum the
 * model can hold, so no series the runtime creates is ever classified by it, and
 * a row nothing can reach is a row that lies about the taxonomy.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Series Classification Rules ---'

insert into ores_marketdata_series_classification_rules_tbl (
    tenant_id, instrument, quote, key_kind, version, asset_class_source, asset_class_code,
    series_subclass_code, description,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- ─── FX ──────────────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'fx_spot', '', 'datum', 0, 'literal', 'fx', 'spot',
     'The spot rate of a currency pair: the class at its most direct, and the subclass names the point the key carries.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'fx_fwd', '', 'datum', 0, 'literal', 'fx', 'forward',
     'A currency pair quoted at a forward tenor, so the class keeps its own name and the subclass records the tenor.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'fx_option', '', 'datum', 0, 'literal', 'fx', 'volatility',
     'A volatility quoted per currency pair, expiry and strike, which measures the class rather than quoting its level.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),

    -- ─── RATES: curves ───────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'discount', '', 'datum', 0, 'literal', 'interest_rates', 'yield',
     'A discount curve quoted per currency and curve id; the curve is the class, and the subclass records that it quotes a level.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'zero', '', 'datum', 0, 'literal', 'interest_rates', 'yield',
     'A zero rate quoted per currency, curve id and day count: the same curve as a discount curve, seen in another basis.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'mm', '', 'datum', 0, 'literal', 'interest_rates', 'yield',
     'A money-market deposit rate quoted per currency and index tenor, the short end of the same curve.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'mm_future', '', 'datum', 0, 'literal', 'interest_rates', 'fra',
     'A listed future on a money-market index. It prices the same forward rate a FRA does, which is the subclass it takes.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'fra', '', 'datum', 0, 'literal', 'interest_rates', 'fra',
     'A forward rate agreement quoted per currency and start tenor, the forward rate the subclass names.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'imm_fra', '', 'datum', 0, 'literal', 'interest_rates', 'fra',
     'A FRA whose start date is an IMM date, which is a convention of the key and not a different rate.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'ir_swap', '', 'datum', 0, 'literal', 'interest_rates', 'yield',
     'The par swap rate of a currency and index tenor, which is the class benchmark rather than a spread over it.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),

    -- ─── RATES: spreads ──────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'basis_swap', '', 'datum', 0, 'literal', 'interest_rates', 'basis',
     'A spread between two floating tenors of one currency, so it measures the basis between them and not a level.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'bma_swap', '', 'datum', 0, 'literal', 'interest_rates', 'basis',
     'A ratio between a tax-exempt index and a taxable one, the municipal analogue of a basis spread.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'cc_basis_swap', '', 'datum', 0, 'literal', 'interest_rates', 'xccy',
     'A basis spread across two currencies. The key names both, so the subclass records the cross-currency nature.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'cc_fix_float_swap', '', 'datum', 0, 'literal', 'interest_rates', 'xccy',
     'A spread on a swap that pays fixed in one currency and floats in another, the same cross-currency case.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),

    -- ─── RATES: vol surfaces ─────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'swaption', '', 'datum', 0, 'literal', 'interest_rates', 'volatility',
     'A volatility quoted per currency, expiry, swap tenor and strike, which measures the class rather than its level.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'capfloor', '', 'datum', 0, 'literal', 'interest_rates', 'volatility',
     'A cap or floor volatility quoted per currency, maturity and strike: the same class and a different instrument.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),

    -- ─── CREDIT ──────────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'hazard_rate', '', 'datum', 0, 'literal', 'credit', 'spread',
     'A default-intensity curve bootstrapped from CDS quotes, which carries the subclass of the spread it came from.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'cds', '', 'datum', 0, 'literal', 'credit', 'spread',
     'A single-name default swap spread quoted per entity, seniority, currency and tenor, the class at its most direct.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'cds_index', '', 'datum', 0, 'literal', 'credit', 'index_credit',
     'A spread on a basket of names rather than one entity, which is what the index subclass records.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'index_cds_option', '', 'datum', 0, 'literal', 'credit', 'index_credit',
     'An option on an index whose key names no single entity, so it takes the index subclass too.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'recovery_rate', '', 'datum', 0, 'literal', 'credit', 'recovery',
     'The assumed recovery on default quoted per entity and seniority, which the subclass names.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'rating', '', 'datum', 0, 'literal', 'credit', 'transition_probability',
     'A rating agency migration matrix: the probability of moving between two grades by a tenor, not a traded level.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'index_cds_tranche', '', 'datum', 0, 'literal', 'credit', 'correlation',
     'The base correlation of a tranche. The names behind it are credit names, but the quantity is a correlation.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'cpr', '', 'datum', 0, 'literal', 'bond', 'prepayment',
     'A conditional prepayment rate on a mortgage security: the key names an ISIN, so the bond class holds it and the subclass is the prepayment.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),

    -- ─── EQUITY ──────────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'equity_spot', '', 'datum', 0, 'literal', 'equity', 'spot',
     'A spot price quoted per name, the class at its most direct.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'equity_fwd', '', 'datum', 0, 'literal', 'equity', 'forward',
     'A forward price quoted per name and tenor.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'equity_dividend', '', 'datum', 0, 'literal', 'equity', 'forward',
     'A dividend forecast quoted as a rate to a tenor, which is a forward on the class rather than a level of it.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'equity_option', '', 'datum', 0, 'literal', 'equity', 'volatility',
     'A volatility quoted per name, expiry and strike, which measures the class rather than quoting its level.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),

    -- ─── COMMODITY ───────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'commodity_spot', '', 'datum', 0, 'literal', 'commodity', 'spot',
     'A spot price quoted per commodity name, the class at its most direct.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'commodity_fwd', '', 'datum', 0, 'literal', 'commodity', 'forward',
     'A forward price quoted per name, currency and tenor.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'commodity_option', '', 'datum', 0, 'literal', 'commodity', 'volatility',
     'A volatility quoted per name, expiry and strike, which measures the class rather than quoting its level.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'oi_future', '', 'datum', 0, 'literal', 'commodity', 'forward',
     'A listed future on an overnight index, which quotes a forward level for a contract month and exchange.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'shape_profile', '', 'datum', 0, 'literal', 'commodity', 'seasonality',
     'A named power price shape: the within-year pattern the class carries, which is what the subclass names.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),

    -- ─── INFLATION ───────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'zc_inflationswap', '', 'datum', 0, 'literal', 'inflation', 'swap',
     'A zero-coupon inflation swap quoted per index and tenor, the class at its most direct.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'yy_inflationswap', '', 'datum', 0, 'literal', 'inflation', 'swap',
     'A year-on-year inflation swap quoted per index and tenor: the same class in a different payoff.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'zc_inflationcapfloor', '', 'datum', 0, 'literal', 'inflation', 'capfloor',
     'A zero-coupon cap or floor quoted per index, maturity and strike, which is the option subclass of the class.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'yy_inflationcapfloor', '', 'datum', 0, 'literal', 'inflation', 'capfloor',
     'A year-on-year cap or floor: the same option subclass in a different payoff.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'seasonality', '', 'datum', 0, 'literal', 'inflation', 'seasonality',
     'The within-year adjustment applied to an inflation index, which is what the subclass names.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),

    -- ─── BOND ────────────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'bond', '', 'datum', 0, 'literal', 'bond', 'price',
     'A bond quoted per issuer and seniority. The PRICE and YIELD_SPREAD metrics both measure the security, and the subclass records that.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),
    (ores_utility_system_tenant_id_fn(), 'bond_option', '', 'datum', 0, 'literal', 'bond', 'volatility',
     'An option on a bond quoted per underlying, expiry and strike, which measures the class rather than quoting its price.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),

    -- ─── FIXINGS (index series) ──────────────────────────────────────────────
    -- ─── INDEX SERIES (fixings) ─────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), '', '', 'index', 0, 'index_authority', null, 'index_fixing',
     'A fixing series is an index and not a datum. One row serves every index family: the class is the refdata class the family carries, so a power fixing is commodity and an equity fixing is equity, and the subclass records the fixing.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules'),

    -- ─── CROSS_ASSET ─────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'correlation', '', 'datum', 0, 'correlation_operands', null, 'correlation',
     'The first two qualifier segments are the operands, and they name the classes the pair relates, so this row names none: a pairwise correlation belongs to both and to neither alone. The subclass is the correlation itself.',
     current_user, current_user, 'system.initial_load', 'Initial population of series classification rules')
on conflict (tenant_id, key_kind, instrument, quote)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'marketdata_series_classification_rules' as entity, count(*) as count
from ores_marketdata_series_classification_rules_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
