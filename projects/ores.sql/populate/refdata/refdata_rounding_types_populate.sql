/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
 * Rounding Types Population Script
 *
 * Populates valid rounding methods per ORE XML schema (roundingType).
 * Values match xs:enumeration in ORE's currencyconfig.xsd.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Rounding Types ---'

-- Update descriptions to include numeric examples (idempotent)
update ores_refdata_rounding_types_tbl
set description = case code
    when 'Up'      then 'Rounds away from zero. 2.341→2.35, 2.349→2.35. Negative: -2.341→-2.35. Used when amounts must never be understated.'
    when 'Down'    then 'Truncates toward zero. 2.349→2.34, -2.341→-2.34. Never overstates amounts; used in conservative contexts.'
    when 'Closest' then 'Round half up to nearest. 2.344→2.34, 2.345→2.35. Default rounding; most common for financial amounts.'
    when 'Floor'   then 'Always rounds to next lower value. 2.349→2.34, -2.341→-2.35. Used when rounding must never produce a higher value.'
    when 'Ceiling' then 'Always rounds to next higher value. 2.341→2.35, -2.349→-2.34. Used when rounding must never produce a lower value.'
    else description
end
where code in ('Up', 'Down', 'Closest', 'Floor', 'Ceiling')
  and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_refdata_rounding_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'Up', 0, 'Round Up',
     'Rounds away from zero. 2.341→2.35, 2.349→2.35. Negative: -2.341→-2.35. Used when amounts must never be understated.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of rounding types'),
    (ores_iam_system_tenant_id_fn(), 'Down', 0, 'Round Down',
     'Truncates toward zero. 2.349→2.34, -2.341→-2.34. Never overstates amounts; used in conservative contexts.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of rounding types'),
    (ores_iam_system_tenant_id_fn(), 'Closest', 0, 'Round to Nearest',
     'Round half up to nearest. 2.344→2.34, 2.345→2.35. Default rounding; most common for financial amounts.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of rounding types'),
    (ores_iam_system_tenant_id_fn(), 'Floor', 0, 'Floor',
     'Always rounds to next lower value. 2.349→2.34, -2.341→-2.35. Used when rounding must never produce a higher value.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of rounding types'),
    (ores_iam_system_tenant_id_fn(), 'Ceiling', 0, 'Ceiling',
     'Always rounds to next higher value. 2.341→2.35, -2.349→-2.34. Used when rounding must never produce a lower value.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of rounding types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_rounding_types' as entity, count(*) as count
from ores_refdata_rounding_types_tbl;
