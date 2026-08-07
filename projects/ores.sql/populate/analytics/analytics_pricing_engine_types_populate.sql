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
 * Pricing Engine Types Population Script
 *
 * Seeds the database with all ORE pricing engine type codes derived from
 * pricingengine.xml Product/@type values across the ORE examples.
 * instrument_type_code links to ores_trading_trade_types_tbl where applicable;
 * NULL for coupon/leg-level pricers that have no corresponding trade type.
 * This script is idempotent.
 */

\echo '--- Pricing Engine Types ---'

insert into ores_analytics_pricing_engine_types_tbl (
    code, tenant_id, version, description, instrument_type_code,
    modified_by, change_reason_code, change_commentary
) values
    -- Interest Rate Products
    ('Swap',                    ores_utility_system_tenant_id_fn(), 0, 'Interest Rate Swap',
     'IRS', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CrossCurrencySwap',       ores_utility_system_tenant_id_fn(), 0, 'Cross Currency Swap',
     'XCCY', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('EuropeanSwaption',        ores_utility_system_tenant_id_fn(), 0, 'European-style Swaption',
     'SWPTN', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('BermudanSwaption',        ores_utility_system_tenant_id_fn(), 0, 'Bermudan-style Swaption',
     'SWPTN', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('AmericanSwaption',        ores_utility_system_tenant_id_fn(), 0, 'American-style Swaption',
     'SWPTN', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CapFloor',                ores_utility_system_tenant_id_fn(), 0, 'Cap or Floor',
     'CAPFLR', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('FlexiSwap',               ores_utility_system_tenant_id_fn(), 0, 'Flexi Swap',
     'FLEXSWP', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('BalanceGuaranteedSwap',   ores_utility_system_tenant_id_fn(), 0, 'Balance Guaranteed Swap',
     'BGS', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CallableSwap',            ores_utility_system_tenant_id_fn(), 0, 'Callable Swap',
     'CALLSWP', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- Coupon/Leg-Level Pricers (no direct instrument type mapping)
    ('YYCapFloor',              ores_utility_system_tenant_id_fn(), 0, 'Year-on-Year Inflation Cap/Floor',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CpiCapFloor',             ores_utility_system_tenant_id_fn(), 0, 'CPI Cap/Floor',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CPICapFloor',             ores_utility_system_tenant_id_fn(), 0, 'CPI Cap/Floor (alternative)',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CappedFlooredCpiLegCoupons', ores_utility_system_tenant_id_fn(), 0, 'Capped/Floored CPI Leg Coupons',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CappedFlooredCpiLegCashFlows', ores_utility_system_tenant_id_fn(), 0, 'Capped/Floored CPI Leg Cash Flows',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CapFlooredIborLeg',       ores_utility_system_tenant_id_fn(), 0, 'Cap/Floored IBOR Leg Pricer',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CapFlooredOvernightIndexedCouponLeg', ores_utility_system_tenant_id_fn(), 0, 'Cap/Floored Overnight Indexed Coupon Leg',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CapFlooredAverageONIndexedCouponLeg', ores_utility_system_tenant_id_fn(), 0, 'Cap/Floored Average ON Indexed Coupon Leg',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CapFlooredYYLeg',         ores_utility_system_tenant_id_fn(), 0, 'Capped/Floored YY Inflation Leg',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CMS',                     ores_utility_system_tenant_id_fn(), 0, 'Constant Maturity Swap Coupon Pricer',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CMSSpread',               ores_utility_system_tenant_id_fn(), 0, 'CMS Spread Coupon Pricer',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('FormulaBasedCoupon',      ores_utility_system_tenant_id_fn(), 0, 'Formula-Based Coupon Pricer',
     'NONE', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- FX Products
    ('FxForward',               ores_utility_system_tenant_id_fn(), 0, 'FX Forward',
     'FXFWD', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('FxOption',                ores_utility_system_tenant_id_fn(), 0, 'FX Option (European)',
     'FXOPT', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('FxOptionAmerican',        ores_utility_system_tenant_id_fn(), 0, 'FX Option (American)',
     'FXOPT', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- Equity Products
    ('EquityOption',            ores_utility_system_tenant_id_fn(), 0, 'Equity Option (European)',
     'EQOPT', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('EquityOptionAmerican',    ores_utility_system_tenant_id_fn(), 0, 'Equity Option (American)',
     'EQOPT', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('EquityForward',           ores_utility_system_tenant_id_fn(), 0, 'Equity Forward',
     'EQFWD', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('EquityBarrierOption',     ores_utility_system_tenant_id_fn(), 0, 'Equity Barrier Option',
     'EQBAR', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('EquityFutureOption',      ores_utility_system_tenant_id_fn(), 0, 'Equity Future Option',
     'EQFUTOPT', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- Credit Products
    ('Bond',                    ores_utility_system_tenant_id_fn(), 0, 'Fixed Income Bond',
     'BOND', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('ForwardBond',             ores_utility_system_tenant_id_fn(), 0, 'Forward Bond',
     'FWDBOND', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('BondOption',              ores_utility_system_tenant_id_fn(), 0, 'Bond Option',
     'BONDOPT', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('BondTRS',                 ores_utility_system_tenant_id_fn(), 0, 'Bond Total Return Swap',
     'BONDTRS', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CreditDefaultSwap',      ores_utility_system_tenant_id_fn(), 0, 'Credit Default Swap',
     'CDS', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('IndexCreditDefaultSwap',  ores_utility_system_tenant_id_fn(), 0, 'Index Credit Default Swap',
     'CDX', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CreditDefaultSwapOption', ores_utility_system_tenant_id_fn(), 0, 'CDS Option',
     'CDSOPT', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('IndexCreditDefaultSwapOption', ores_utility_system_tenant_id_fn(), 0, 'Index CDS Option',
     'CDXOPT', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('SyntheticCDO',            ores_utility_system_tenant_id_fn(), 0, 'Synthetic CDO',
     'CDO', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- Commodity Products
    ('CommodityForward',        ores_utility_system_tenant_id_fn(), 0, 'Commodity Forward',
     'COMMFWD', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CommoditySwap',           ores_utility_system_tenant_id_fn(), 0, 'Commodity Swap',
     'COMMSWP', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CommodityOption',         ores_utility_system_tenant_id_fn(), 0, 'Commodity Option',
     'COMMOPT', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CommodityAveragePriceOption', ores_utility_system_tenant_id_fn(), 0, 'Commodity Average Price Option',
     'COMMAPO', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CommoditySwaption',       ores_utility_system_tenant_id_fn(), 0, 'Commodity Swaption',
     'COMMSWPTN', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- Scripted/Generic
    ('ScriptedTrade',           ores_utility_system_tenant_id_fn(), 0, 'Scripted Trade (ORE generic payoff engine)',
     'SCRIPT', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types')

on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Pricing Engine Types' as entity, count(*) as count
from ores_analytics_pricing_engine_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
