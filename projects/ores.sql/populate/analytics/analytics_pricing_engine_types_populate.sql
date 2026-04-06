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
    ('Swap',                    ores_iam_system_tenant_id_fn(), 0, 'Interest Rate Swap',
     'Swap', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CrossCurrencySwap',       ores_iam_system_tenant_id_fn(), 0, 'Cross Currency Swap',
     'CrossCurrencySwap', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('EuropeanSwaption',        ores_iam_system_tenant_id_fn(), 0, 'European-style Swaption',
     'Swaption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('BermudanSwaption',        ores_iam_system_tenant_id_fn(), 0, 'Bermudan-style Swaption',
     'Swaption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('AmericanSwaption',        ores_iam_system_tenant_id_fn(), 0, 'American-style Swaption',
     'Swaption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CapFloor',                ores_iam_system_tenant_id_fn(), 0, 'Cap or Floor',
     'CapFloor', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('FlexiSwap',               ores_iam_system_tenant_id_fn(), 0, 'Flexi Swap',
     'FlexiSwap', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('BalanceGuaranteedSwap',   ores_iam_system_tenant_id_fn(), 0, 'Balance Guaranteed Swap',
     'BalanceGuaranteedSwap', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CallableSwap',            ores_iam_system_tenant_id_fn(), 0, 'Callable Swap',
     'CallableSwap', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('MultiLegOption',          ores_iam_system_tenant_id_fn(), 0, 'Multi-Leg Option',
     'MultiLegOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- Coupon/Leg-Level Pricers (no corresponding instrument type)
    ('YYCapFloor',              ores_iam_system_tenant_id_fn(), 0, 'Year-on-Year Inflation Cap/Floor',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CpiCapFloor',             ores_iam_system_tenant_id_fn(), 0, 'CPI Cap/Floor',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CPICapFloor',             ores_iam_system_tenant_id_fn(), 0, 'CPI Cap/Floor (alternative)',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CappedFlooredCpiLegCoupons', ores_iam_system_tenant_id_fn(), 0, 'Capped/Floored CPI Leg Coupons',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CappedFlooredCpiLegCashFlows', ores_iam_system_tenant_id_fn(), 0, 'Capped/Floored CPI Leg Cash Flows',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CapFlooredIborLeg',       ores_iam_system_tenant_id_fn(), 0, 'Cap/Floored IBOR Leg Pricer',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CapFlooredOvernightIndexedCouponLeg', ores_iam_system_tenant_id_fn(), 0, 'Cap/Floored Overnight Indexed Coupon Leg',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CapFlooredAverageONIndexedCouponLeg', ores_iam_system_tenant_id_fn(), 0, 'Cap/Floored Average ON Indexed Coupon Leg',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CapFlooredYYLeg',         ores_iam_system_tenant_id_fn(), 0, 'Capped/Floored YY Inflation Leg',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CMS',                     ores_iam_system_tenant_id_fn(), 0, 'Constant Maturity Swap Coupon Pricer',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CMSSpread',               ores_iam_system_tenant_id_fn(), 0, 'CMS Spread Coupon Pricer',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('FormulaBasedCoupon',      ores_iam_system_tenant_id_fn(), 0, 'Formula-Based Coupon Pricer',
     null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- FX Products
    ('FxForward',               ores_iam_system_tenant_id_fn(), 0, 'FX Forward',
     'FxForward', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('FxOption',                ores_iam_system_tenant_id_fn(), 0, 'FX Option (European)',
     'FxOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('FxOptionAmerican',        ores_iam_system_tenant_id_fn(), 0, 'FX Option (American)',
     'FxOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- Equity Products
    ('EquityOption',            ores_iam_system_tenant_id_fn(), 0, 'Equity Option (European)',
     'EquityOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('EquityOptionAmerican',    ores_iam_system_tenant_id_fn(), 0, 'Equity Option (American)',
     'EquityOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('EquityForward',           ores_iam_system_tenant_id_fn(), 0, 'Equity Forward',
     'EquityForward', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('EquityBarrierOption',     ores_iam_system_tenant_id_fn(), 0, 'Equity Barrier Option',
     'EquityBarrierOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('EquityFutureOption',      ores_iam_system_tenant_id_fn(), 0, 'Equity Future Option',
     'EquityFutureOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- Credit Products
    ('Bond',                    ores_iam_system_tenant_id_fn(), 0, 'Fixed Income Bond',
     'Bond', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('ForwardBond',             ores_iam_system_tenant_id_fn(), 0, 'Forward Bond',
     'ForwardBond', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('BondOption',              ores_iam_system_tenant_id_fn(), 0, 'Bond Option',
     'BondOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('BondTRS',                 ores_iam_system_tenant_id_fn(), 0, 'Bond Total Return Swap',
     'BondTRS', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CreditDefaultSwap',      ores_iam_system_tenant_id_fn(), 0, 'Credit Default Swap',
     'CreditDefaultSwap', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('IndexCreditDefaultSwap',  ores_iam_system_tenant_id_fn(), 0, 'Index Credit Default Swap',
     'IndexCreditDefaultSwap', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CreditDefaultSwapOption', ores_iam_system_tenant_id_fn(), 0, 'CDS Option',
     'CreditDefaultSwapOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('IndexCreditDefaultSwapOption', ores_iam_system_tenant_id_fn(), 0, 'Index CDS Option',
     'IndexCreditDefaultSwapOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('SyntheticCDO',            ores_iam_system_tenant_id_fn(), 0, 'Synthetic CDO',
     'SyntheticCDO', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- Commodity Products
    ('CommodityForward',        ores_iam_system_tenant_id_fn(), 0, 'Commodity Forward',
     'CommodityForward', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CommoditySwap',           ores_iam_system_tenant_id_fn(), 0, 'Commodity Swap',
     'CommoditySwap', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CommodityOption',         ores_iam_system_tenant_id_fn(), 0, 'Commodity Option',
     'CommodityOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CommodityAveragePriceOption', ores_iam_system_tenant_id_fn(), 0, 'Commodity Average Price Option',
     'CommodityAveragePriceOption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),
    ('CommoditySwaption',       ores_iam_system_tenant_id_fn(), 0, 'Commodity Swaption',
     'CommoditySwaption', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types'),

    -- Scripted/Generic
    ('ScriptedTrade',           ores_iam_system_tenant_id_fn(), 0, 'Scripted Trade (ORE generic payoff engine)',
     'ScriptedTrade', 'ores_analytics_service', 'system.initial_load', 'Seed pricing engine types')

on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Pricing Engine Types' as entity, count(*) as count
from ores_analytics_pricing_engine_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
