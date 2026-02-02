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

insert into ores_refdata_rounding_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'Up', 0, 'Round Up',
     'Rounds away from zero toward positive infinity. Positive values round up, negative values round to larger absolute value.',
     1, current_user, 'system.initial_load', 'Initial population of rounding types'),
    (ores_iam_system_tenant_id_fn(), 'Down', 0, 'Round Down',
     'Rounds toward zero (truncation). Drops fractional digits without rounding, always reducing absolute value.',
     2, current_user, 'system.initial_load', 'Initial population of rounding types'),
    (ores_iam_system_tenant_id_fn(), 'Closest', 0, 'Round to Nearest',
     'Rounds to the nearest value. When equidistant, uses default tie-breaking (typically banker''s rounding or round half up).',
     3, current_user, 'system.initial_load', 'Initial population of rounding types'),
    (ores_iam_system_tenant_id_fn(), 'Floor', 0, 'Floor',
     'Rounds toward negative infinity. Always rounds to the next lower value regardless of sign.',
     4, current_user, 'system.initial_load', 'Initial population of rounding types'),
    (ores_iam_system_tenant_id_fn(), 'Ceiling', 0, 'Ceiling',
     'Rounds toward positive infinity. Always rounds to the next higher value regardless of sign.',
     5, current_user, 'system.initial_load', 'Initial population of rounding types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_rounding_types' as entity, count(*) as count
from ores_refdata_rounding_types_tbl;
