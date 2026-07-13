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
 * Tenor Units Population Script
 *
 * Populates the valid tenor period units (DAY, WEEK, MONTH, YEAR), plus
 * the NONE sentinel used by SPECIAL tenors, which have no fixed unit of
 * their own.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Tenor Units ---'

insert into ores_refdata_tenor_units_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'DAY', 0, 'Day',
     'Period unit measured in days.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of tenor units'),
    (ores_utility_system_tenant_id_fn(), 'WEEK', 0, 'Week',
     'Period unit measured in weeks.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of tenor units'),
    (ores_utility_system_tenant_id_fn(), 'MONTH', 0, 'Month',
     'Period unit measured in months.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of tenor units'),
    (ores_utility_system_tenant_id_fn(), 'YEAR', 0, 'Year',
     'Period unit measured in years.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of tenor units'),
    (ores_utility_system_tenant_id_fn(), 'NONE', 0, 'None',
     'Sentinel for SPECIAL tenors, which have no fixed unit/multiplier of their own.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of tenor units')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_tenor_units' as entity, count(*) as count
from ores_refdata_tenor_units_tbl;
