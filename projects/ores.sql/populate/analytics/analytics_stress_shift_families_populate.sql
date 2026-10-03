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
 * Stress Shift Families Population Script
 *
 * Seeds the shift families of an ORE stress scenario: the children of the
 * stresstest type in external/ore/xsd/stress.xsd, apart from Date.
 * This script is idempotent.
 */

\echo '--- Stress Shift Families ---'

insert into ores_analytics_stress_shift_families_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('ParShifts', ores_utility_system_tenant_id_fn(), 0, 'Par rate shifts applied before the zero rate shifts.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('DiscountCurves', ores_utility_system_tenant_id_fn(), 0, 'Shifts to discount curves, by currency.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('IndexCurves', ores_utility_system_tenant_id_fn(), 0, 'Shifts to index forwarding curves, by index.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('YieldCurves', ores_utility_system_tenant_id_fn(), 0, 'Shifts to named yield curves.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('FxSpots', ores_utility_system_tenant_id_fn(), 0, 'Shifts to FX spot rates, by currency pair.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('FxVolatilities', ores_utility_system_tenant_id_fn(), 0, 'Shifts to FX volatility surfaces.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('SwaptionVolatilities', ores_utility_system_tenant_id_fn(), 0, 'Shifts to swaption volatility surfaces.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('CapFloorVolatilities', ores_utility_system_tenant_id_fn(), 0, 'Shifts to cap and floor volatility surfaces.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('EquitySpots', ores_utility_system_tenant_id_fn(), 0, 'Shifts to equity spot prices.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('EquityVolatilities', ores_utility_system_tenant_id_fn(), 0, 'Shifts to equity volatility surfaces.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('CommodityCurves', ores_utility_system_tenant_id_fn(), 0, 'Shifts to commodity price curves.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('IntradayPowerCurves', ores_utility_system_tenant_id_fn(), 0, 'Shifts to intraday power price curves.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('CommodityVolatilities', ores_utility_system_tenant_id_fn(), 0, 'Shifts to commodity volatility surfaces.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('SecuritySpreads', ores_utility_system_tenant_id_fn(), 0, 'Shifts to bond security spreads.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('RecoveryRates', ores_utility_system_tenant_id_fn(), 0, 'Shifts to recovery rates.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families'),
    ('SurvivalProbabilities', ores_utility_system_tenant_id_fn(), 0, 'Shifts to survival probabilities.',
     'ores_analytics_service', 'system.initial_load', 'Seed stress shift families')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
