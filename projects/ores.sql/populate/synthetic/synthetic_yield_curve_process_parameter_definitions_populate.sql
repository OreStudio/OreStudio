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
 * Yield Curve Process Parameter Definitions Population Script
 *
 * Populates the parameter-definition catalogue for every yield curve
 * process type. Each row declares one parameter: its display name,
 * user-facing description, data type, default value, and (where the
 * model requires it) min/max bounds. The description is the text shown
 * in the UI; the bounds are enforced both by the UI spin boxes and by
 * the mapping layer (map_parameters_to_yield_curve_process) at
 * process-construction time.
 *
 * Managed by the system tenant as read-only reference data; tenant
 * users never edit definitions, only the values of their own configs.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Yield Curve Process Parameter Definitions ---'

insert into ores_synthetic_yield_curve_process_parameter_definitions_tbl (
    tenant_id, id, version, process_type_code, parameter_name, description,
    data_type, default_value, min_value, max_value, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- Vasicek
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0, 'VASICEK', 'kappa',
     'Mean reversion speed: how strongly the short rate is pulled back toward the long-term mean theta. Higher values mean the rate reverts faster, giving a shorter effective memory of its initial level. Must be non-negative.',
     'double', 0.5, 0, null, 1, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'VASICEK', 'theta',
     'Long-term mean: the level the short rate reverts to in the long run, and the level around which the generated yield curve anchors.',
     'double', 0.03, null, null, 2, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'VASICEK', 'sigma',
     'Volatility: the constant annualised standard deviation of the short-rate increments. Must be non-negative.',
     'double', 0.01, 0, null, 3, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'VASICEK', 'initial_rate',
     'Initial short rate: the level of the short rate at the simulation start date (time zero).',
     'double', 0.03, null, null, 4, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),

    -- Cox-Ingersoll-Ross
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'COX_INGERSOLL_ROSS', 'kappa',
     'Mean reversion speed: how strongly the short rate is pulled back toward the long-term mean theta. Must be non-negative.',
     'double', 0.5, 0, null, 1, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'COX_INGERSOLL_ROSS', 'theta',
     'Long-term mean: the level the short rate reverts to in the long run. Combined with kappa and sigma it determines the long-run stationary distribution of the rate.',
     'double', 0.03, null, null, 2, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'COX_INGERSOLL_ROSS', 'sigma',
     'Volatility: the annualised standard deviation of the short-rate increments, scaled by the square root of the current rate. Must be non-negative.',
     'double', 0.01, 0, null, 3, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'COX_INGERSOLL_ROSS', 'initial_rate',
     'Initial short rate: the level of the short rate at the simulation start date (time zero). The model keeps the rate non-negative, so the initial rate must be non-negative.',
     'double', 0.03, 0, null, 4, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),

    -- Hull-White
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'HULL_WHITE', 'kappa',
     'Mean reversion speed: how strongly the short rate is pulled back toward the time-dependent target theta(t). Must be non-negative.',
     'double', 0.5, 0, null, 1, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'HULL_WHITE', 'theta',
     'Long-term mean: the mean-reversion target around which the short rate evolves (held constant for the generated curve).',
     'double', 0.03, null, null, 2, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'HULL_WHITE', 'sigma',
     'Volatility: the constant annualised standard deviation of the short-rate increments. Must be non-negative.',
     'double', 0.01, 0, null, 3, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'HULL_WHITE', 'initial_rate',
     'Initial short rate: the level of the short rate at the simulation start date (time zero).',
     'double', 0.03, null, null, 4, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),

    -- Black-Karasinski
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'BLACK_KARASINSKI', 'kappa',
     'Mean reversion speed of ln r: how strongly the logarithm of the short rate is pulled back toward its long-run level theta. kappa <= 0 is a valid, if degenerate, driftless case (a random walk of ln r).',
     'double', 0.5, null, null, 1, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'BLACK_KARASINSKI', 'theta',
     'Long-run level of ln r: the level the logarithm of the short rate reverts to in the long run. The model is written on the log rate, so this is a logarithm, not a rate: a long-run rate of 4% corresponds to log(0.04) = -3.218876.',
     'double', -3.218876, null, null, 2, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'BLACK_KARASINSKI', 'sigma',
     'Volatility of ln r: the constant annualised standard deviation of the log-rate increments. Must be non-negative.',
     'double', 0.01, 0, null, 3, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'BLACK_KARASINSKI', 'initial_rate',
     'Initial short rate: the level of the short rate at the simulation start date (time zero). Must be strictly positive - the model runs on the logarithm of the rate.',
     'double', 0.03, 0, null, 4, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),

    -- Two-Factor Gaussian (G2++)
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'TWO_FACTOR_GAUSSIAN', 'kappa_x',
     'Mean reversion speed of the first factor: how strongly factor x is pulled back toward zero. Combined with kappa_y it shapes the hump of the initial term structure. Must be non-negative.',
     'double', 0.5, 0, null, 1, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'TWO_FACTOR_GAUSSIAN', 'kappa_y',
     'Mean reversion speed of the second factor: how strongly factor y is pulled back toward zero. Typically smaller than kappa_x, giving the second factor a longer memory. Must be non-negative.',
     'double', 0.3, 0, null, 2, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'TWO_FACTOR_GAUSSIAN', 'theta',
     'Long-term mean: the level the short rate (the sum of the two factors plus theta) reverts to in the long run.',
     'double', 0.03, null, null, 3, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'TWO_FACTOR_GAUSSIAN', 'sigma_x',
     'Volatility of the first factor: the annualised standard deviation of factor x increments. Must be non-negative.',
     'double', 0.01, 0, null, 4, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'TWO_FACTOR_GAUSSIAN', 'sigma_y',
     'Volatility of the second factor: the annualised standard deviation of factor y increments. Must be non-negative.',
     'double', 0.005, 0, null, 5, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'TWO_FACTOR_GAUSSIAN', 'rho',
     'Correlation between the two factors: how closely the two Ornstein-Uhlenbeck factors move together. Must lie between -1 (perfect negative correlation) and 1 (perfect positive correlation).',
     'double', 0, -1, 1, 6, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions'),
    (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0,'TWO_FACTOR_GAUSSIAN', 'initial_rate',
     'Initial short rate: the level of the short rate at the simulation start date (time zero).',
     'double', 0.03, null, null, 7, current_user, current_user, 'system.initial_load',
     'Initial population of yield curve process parameter definitions')
on conflict (tenant_id, process_type_code, parameter_name)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'synthetic_yield_curve_process_parameter_definitions' as entity, count(*) as count
from ores_synthetic_yield_curve_process_parameter_definitions_tbl;
