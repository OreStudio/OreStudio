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
 * Pricing Pair Settings Population Script
 *
 * Seeds the settings that a pair of model and engine is known to take, with
 * the kind of each setting, its allowed values and its usual value.
 * This script is idempotent.
 */

\echo '--- Pricing Pair Settings ---'

insert into ores_analytics_pricing_pair_settings_tbl (
    code, tenant_id, version, model, engine, pricing_engine_type_code, scope, name, kind,
    usual_value, options, note, modified_by, change_reason_code, change_commentary
) values
    ('BermudanSwaption:LGM:Grid:model:Calibration', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', 'model', 'Calibration', 'select', 'Bootstrap', 'Bootstrap,BestFit,None', 'How the model is fitted to the swaption quotes.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('BermudanSwaption:LGM:Grid:model:CalibrationStrategy', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', 'model', 'CalibrationStrategy', 'select', 'CoterminalATM', 'CoterminalATM,CoterminalDealStrike,UnderlyingATM,UnderlyingDealStrike', 'Which swaptions the fit uses.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('BermudanSwaption:LGM:Grid:model:ReversionType', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', 'model', 'ReversionType', 'select', 'HullWhite', 'HullWhite,Hagan', 'The parameterisation of mean reversion.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('BermudanSwaption:LGM:Grid:model:VolatilityType', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', 'model', 'VolatilityType', 'select', 'Hagan', 'Hagan,HullWhite', 'The parameterisation of volatility.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('BermudanSwaption:LGM:Grid:model:Volatility', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', 'model', 'Volatility', 'number', '0.01', null, 'The starting volatility the fit moves from.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('BermudanSwaption:LGM:Grid:model:ShiftHorizon', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', 'model', 'ShiftHorizon', 'number', '0.5', null, 'The horizon, in years, of the shift of the state variable.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('BermudanSwaption:LGM:Grid:engine:sx', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', 'engine', 'sx', 'number', '3.0', null, 'Width of the grid, in standard deviations, on the state axis.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('BermudanSwaption:LGM:Grid:engine:sy', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', 'engine', 'sy', 'number', '3.0', null, 'Width of the grid, in standard deviations, on the value axis.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('BermudanSwaption:LGM:Grid:engine:nx', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', 'engine', 'nx', 'number', '10', null, 'Number of grid points on the state axis.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('BermudanSwaption:LGM:Grid:engine:ny', ores_utility_system_tenant_id_fn(), 0, 'LGM', 'Grid', 'BermudanSwaption', 'engine', 'ny', 'number', '10', null, 'Number of grid points on the value axis.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('Bond:DiscountedCashflows:DiscountingRiskyBondEngine:engine:TimestepPeriod', ores_utility_system_tenant_id_fn(), 0, 'DiscountedCashflows', 'DiscountingRiskyBondEngine', 'Bond', 'engine', 'TimestepPeriod', 'text', '6M', null, 'The step of the default probability grid.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings'),
    ('Swap:DiscountedCashflows:DiscountingSwapEngine:engine:SensitivityTemplate', ores_utility_system_tenant_id_fn(), 0, 'DiscountedCashflows', 'DiscountingSwapEngine', 'Swap', 'engine', 'SensitivityTemplate', 'select', 'IR_Analytical', 'IR_Analytical', 'The sensitivity method a swap offers.', 'ores_analytics_service', 'system.initial_load', 'Seed pricing pair settings')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Pricing Pair Settings' as entity, count(*) as count
from ores_analytics_pricing_pair_settings_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
