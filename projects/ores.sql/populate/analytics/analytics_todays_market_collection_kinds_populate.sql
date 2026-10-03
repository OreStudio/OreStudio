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
 * Today's Market Collection Kinds Population Script
 *
 * Seeds the collection kinds of an ORE today's market document: the children
 * of the TodaysMarket element in external/ore/xsd/todaysmarket.xsd, apart from
 * Configuration, each with the element its entries are written as and the
 * attribute or two attributes an entry identifies itself by.
 * This script is idempotent.
 */

\echo '--- Today''s Market Collection Kinds ---'

insert into ores_analytics_todays_market_collection_kinds_tbl (
    code, tenant_id, version, entry_element, key_attribute, key_attribute_2,
    modified_by, change_reason_code, change_commentary
) values
    ('YieldCurves', ores_utility_system_tenant_id_fn(), 0, 'YieldCurve', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('DiscountingCurves', ores_utility_system_tenant_id_fn(), 0, 'DiscountingCurve', 'currency', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('IndexForwardingCurves', ores_utility_system_tenant_id_fn(), 0, 'Index', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('SwapIndexCurves', ores_utility_system_tenant_id_fn(), 0, 'SwapIndex', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('ZeroInflationIndexCurves', ores_utility_system_tenant_id_fn(), 0, 'ZeroInflationIndexCurve', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('YYInflationIndexCurves', ores_utility_system_tenant_id_fn(), 0, 'YYInflationIndexCurve', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('FxSpots', ores_utility_system_tenant_id_fn(), 0, 'FxSpot', 'pair', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('FxVolatilities', ores_utility_system_tenant_id_fn(), 0, 'FxVolatility', 'pair', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('SwaptionVolatilities', ores_utility_system_tenant_id_fn(), 0, 'SwaptionVolatility', 'key', 'currency',
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('YieldVolatilities', ores_utility_system_tenant_id_fn(), 0, 'YieldVolatility', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('CapFloorVolatilities', ores_utility_system_tenant_id_fn(), 0, 'CapFloorVolatility', 'key', 'currency',
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('CDSVolatilities', ores_utility_system_tenant_id_fn(), 0, 'CDSVolatility', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('DefaultCurves', ores_utility_system_tenant_id_fn(), 0, 'DefaultCurve', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('YYInflationCapFloorVolatilities', ores_utility_system_tenant_id_fn(), 0, 'YYInflationCapFloorVolatility', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('ZeroInflationCapFloorVolatilities', ores_utility_system_tenant_id_fn(), 0, 'ZeroInflationCapFloorVolatility', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('EquityCurves', ores_utility_system_tenant_id_fn(), 0, 'EquityCurve', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('EquityVolatilities', ores_utility_system_tenant_id_fn(), 0, 'EquityVolatility', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('Securities', ores_utility_system_tenant_id_fn(), 0, 'Security', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('BaseCorrelations', ores_utility_system_tenant_id_fn(), 0, 'BaseCorrelation', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('CommodityCurves', ores_utility_system_tenant_id_fn(), 0, 'CommodityCurve', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('CommodityVolatilities', ores_utility_system_tenant_id_fn(), 0, 'CommodityVolatility', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('Correlations', ores_utility_system_tenant_id_fn(), 0, 'Correlation', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('BondFutureVolatilities', ores_utility_system_tenant_id_fn(), 0, 'BondFutureVolatility', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds'),
    ('IntradayPowerPriceCurves', ores_utility_system_tenant_id_fn(), 0, 'IntradayPowerPriceCurve', 'name', null,
     'ores_analytics_service', 'system.initial_load', 'Seed today''s market collection kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
