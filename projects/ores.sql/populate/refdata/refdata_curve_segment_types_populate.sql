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
 * Curve Segment Types Population Script
 *
 * Populates the segment types an ORE yield curve is bootstrapped from, each
 * with the segment element it is written under. The set is read from
 * segmentsType in external/ore/xsd/curveconfig.xsd: the Type enumeration of a
 * segment where it has one, and its fixed Type value where it does not.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Curve Segment Types ---'

insert into ores_refdata_curve_segment_types_tbl (
    tenant_id, code, segment_kind, description, version,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'Zero', 'Direct', 'A curve given directly as zero rates.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Discount', 'Direct', 'A curve given directly as discount factors.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Deposit', 'Simple', 'Money market deposits.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'FRA', 'Simple', 'Forward rate agreements.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Future', 'Simple', 'Interest rate futures.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'OIS', 'Simple', 'Overnight index swaps.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Swap', 'Simple', 'Fixed for floating interest rate swaps.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'BMA Basis Swap', 'Simple', 'BMA (SIFMA) against Libor basis swaps.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Average OIS', 'AverageOIS', 'Swaps paying an averaged overnight rate.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Tenor Basis Swap', 'TenorBasis', 'Floating for floating swaps between two tenors of one currency.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Tenor Basis Two Swaps', 'TenorBasis', 'A tenor basis quoted as the difference of two swaps.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Cross Currency Basis Swap', 'CrossCurrency', 'Floating for floating swaps between two currencies.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Cross Currency Fix Float Swap', 'CrossCurrency', 'Fixed for floating swaps between two currencies.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'FX Forward', 'CrossCurrency', 'FX forward points.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Zero Spread', 'ZeroSpread', 'A spread over a reference curve.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Discount Ratio', 'DiscountRatio', 'The ratio of two curves applied to a base curve.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'FittedBond', 'FittedBond', 'A curve fitted to bond prices.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Bond Yield Shifted', 'BondYieldShifted', 'A reference curve shifted by bond yields.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Weighted Average', 'WeightedAverage', 'A weighted average of two reference curves.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Yield Plus Default', 'YieldPlusDefault', 'A reference curve plus one or more default curves.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types'),
    (ores_utility_system_tenant_id_fn(), 'Ibor Fallback', 'IborFallback', 'An Ibor index curve derived from its risk free fallback rate.', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of curve segment types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_curve_segment_types' as entity, count(*) as count
from ores_refdata_curve_segment_types_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
