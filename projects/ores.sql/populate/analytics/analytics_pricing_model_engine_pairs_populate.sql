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
 * Pricing Model Engine Pairs Population Script
 *
 * Seeds the pairs of model and engine that ORE's examples use for each
 * pricing engine type, counted from the pricingengine*.xml files under the
 * ORE Examples folder. The pair used most often for a type is marked usual.
 * This script is idempotent.
 */

\echo '--- Pricing Model Engine Pairs ---'

insert into ores_analytics_pricing_model_engine_pairs_tbl (
    code, tenant_id, version, model, engine, pricing_engine_type_code, is_usual, note,
    modified_by, change_reason_code, change_commentary
) values
    ('Swap:DiscountedCashflows:DiscountingSwapEngine', ores_utility_system_tenant_id_fn(), 0, 'DiscountedCashflows', 'DiscountingSwapEngine', 'Swap', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('CapFlooredIborLeg:BlackOrBachelier:BlackIborCouponPricer', ores_utility_system_tenant_id_fn(), 0, 'BlackOrBachelier', 'BlackIborCouponPricer', 'CapFlooredIborLeg', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('CMS:LinearTSR:LinearTSRPricer', ores_utility_system_tenant_id_fn(), 0, 'LinearTSR', 'LinearTSRPricer', 'CMS', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('EquityOption:BlackScholesMerton:AnalyticEuropeanEngine', ores_utility_system_tenant_id_fn(), 0, 'BlackScholesMerton', 'AnalyticEuropeanEngine', 'EquityOption', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('CommodityForward:DiscountedCashflows:DiscountingCommodityForwardEngine', ores_utility_system_tenant_id_fn(), 0, 'DiscountedCashflows', 'DiscountingCommodityForwardEngine', 'CommodityForward', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('Bond:DiscountedCashflows:DiscountingRiskyBondEngine', ores_utility_system_tenant_id_fn(), 0, 'DiscountedCashflows', 'DiscountingRiskyBondEngine', 'Bond', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('CrossCurrencySwap:DiscountedCashflows:DiscountingCrossCurrencySwapEngine', ores_utility_system_tenant_id_fn(), 0, 'DiscountedCashflows', 'DiscountingCrossCurrencySwapEngine', 'CrossCurrencySwap', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('EuropeanSwaption:BlackBachelier:BlackBachelierSwaptionEngine', ores_utility_system_tenant_id_fn(), 0, 'BlackBachelier', 'BlackBachelierSwaptionEngine', 'EuropeanSwaption', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('FxForward:DiscountedCashflows:DiscountingFxForwardEngine', ores_utility_system_tenant_id_fn(), 0, 'DiscountedCashflows', 'DiscountingFxForwardEngine', 'FxForward', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('FxOption:GarmanKohlhagen:AnalyticEuropeanEngine', ores_utility_system_tenant_id_fn(), 0, 'GarmanKohlhagen', 'AnalyticEuropeanEngine', 'FxOption', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('CreditDefaultSwap:DiscountedCashflows:MidPointCdsEngine', ores_utility_system_tenant_id_fn(), 0, 'DiscountedCashflows', 'MidPointCdsEngine', 'CreditDefaultSwap', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('CapFloor:IborCapModel:IborCapEngine', ores_utility_system_tenant_id_fn(), 0, 'IborCapModel', 'IborCapEngine', 'CapFloor', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('BermudanSwaption:LGM:Grid', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('EquityForward:DiscountedCashflows:DiscountingEquityForwardEngine', ores_utility_system_tenant_id_fn(), 0, 'DiscountedCashflows', 'DiscountingEquityForwardEngine', 'EquityForward', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('EquityOptionAmerican:BlackScholesMerton:FdBlackScholesVanillaEngine', ores_utility_system_tenant_id_fn(), 0, 'BlackScholesMerton', 'FdBlackScholesVanillaEngine', 'EquityOptionAmerican', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('FxOptionAmerican:GarmanKohlhagen:FdBlackScholesVanillaEngine', ores_utility_system_tenant_id_fn(), 0, 'GarmanKohlhagen', 'FdBlackScholesVanillaEngine', 'FxOptionAmerican', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('CMSSpread:BrigoMercurio:Analytic', ores_utility_system_tenant_id_fn(), 0, 'BrigoMercurio', 'Analytic', 'CMSSpread', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('CommoditySwap:DiscountedCashflows:CommoditySwapEngine', ores_utility_system_tenant_id_fn(), 0, 'DiscountedCashflows', 'CommoditySwapEngine', 'CommoditySwap', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('Swap:CrossAssetModel:AMC', ores_utility_system_tenant_id_fn(), 0, 'CrossAssetModel', 'AMC', 'Swap', false, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('CrossCurrencySwap:CrossAssetModel:AMC', ores_utility_system_tenant_id_fn(), 0, 'CrossAssetModel', 'AMC', 'CrossCurrencySwap', false, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('FlexiSwap:LGM:Grid', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'FlexiSwap', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('YYCapFloor:YYCapModel:YYCapEngine', ores_utility_system_tenant_id_fn(), 0, 'YYCapModel', 'YYCapEngine', 'YYCapFloor', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('CpiCapFloor:CpiCapModel:CpiCapEngine', ores_utility_system_tenant_id_fn(), 0, 'CpiCapModel', 'CpiCapEngine', 'CpiCapFloor', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs'),
    ('BondOption:Black:BlackBondOptionEngine', ores_utility_system_tenant_id_fn(), 0, 'Black', 'BlackBondOptionEngine', 'BondOption', true, null, 'ores_analytics_service', 'system.initial_load', 'Seed pricing model engine pairs')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Pricing Model Engine Pairs' as entity, count(*) as count
from ores_analytics_pricing_model_engine_pairs_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
