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
 * Curve Sections Population Script
 *
 * Populates the sections an ORE curveconfig.xml is built from, with the
 * element each section writes its entries as. The set is the children of the
 * curveconfiguration type in external/ore/xsd/curveconfig.xsd, except
 * ReportConfiguration, which holds report settings rather than entries.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Curve Sections ---'

insert into ores_refdata_curve_sections_tbl (
    tenant_id, code, entry_element, description, version,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'FXSpots', 'FXSpot', 'FX spot rates, each named by its currency pair.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'FXVolatilities', 'FXVolatility', 'FX option volatility surfaces.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'SwaptionVolatilities', 'SwaptionVolatility', 'Interest rate swaption volatility surfaces and cubes.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'YieldVolatilities', 'YieldVolatility', 'Bond yield volatility surfaces.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'CapFloorVolatilities', 'CapFloorVolatility', 'Interest rate cap and floor volatility surfaces.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'CDSVolatilities', 'CDSVolatility', 'Credit default swap option volatility surfaces.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'DefaultCurves', 'DefaultCurve', 'Credit default probability curves.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'YieldCurves', 'YieldCurve', 'Interest rate curves, each bootstrapped from a list of segments.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'InflationCurves', 'InflationCurve', 'Zero coupon and year on year inflation curves.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'InflationCapFloorVolatilities', 'InflationCapFloorVolatility', 'Inflation cap and floor volatility surfaces.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'EquityCurves', 'EquityCurve', 'Equity forward curves and dividend yields.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'EquityVolatilities', 'EquityVolatility', 'Equity option volatility surfaces.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'Securities', 'Security', 'Bond securities with their spread, recovery and price quotes.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'BaseCorrelations', 'BaseCorrelation', 'Base correlation curves for credit index tranches.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'CommodityCurves', 'CommodityCurve', 'Commodity price curves.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'CommodityVolatilities', 'CommodityVolatility', 'Commodity option volatility surfaces.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'Correlations', 'Correlation', 'Correlation curves between two indices.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'BondFutureVolatilities', 'BondFutureVolatility', 'Bond future option volatility surfaces.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections'),
    (ores_utility_system_tenant_id_fn(), 'IntradayPowerCurves', 'IntradayPowerCurve', 'Intraday power price curves.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve sections')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_curve_sections' as entity, count(*) as count
from ores_refdata_curve_sections_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
