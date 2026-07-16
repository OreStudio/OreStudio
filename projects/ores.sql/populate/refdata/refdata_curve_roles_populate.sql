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
 * Curve Roles Population Script
 *
 * Populates the valid instrument_code curve-role values (DEPOSIT, FRA,
 * SWAP), plus the NONE sentinel for the great majority of instrument
 * codes that are not curve instruments at all.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Curve Roles ---'

insert into ores_refdata_curve_roles_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'DEPOSIT', 0, 'Deposit',
     'Point instrument priced as a simple deposit rate from a single discount factor.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of curve roles'),
    (ores_utility_system_tenant_id_fn(), 'FRA', 0, 'Forward Rate Agreement',
     'Interval instrument priced as an implied forward rate between two discount factors.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of curve roles'),
    (ores_utility_system_tenant_id_fn(), 'SWAP', 0, 'Swap',
     'Point instrument priced as a par swap rate solved from a fixed-leg discount factor strip.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of curve roles'),
    (ores_utility_system_tenant_id_fn(), 'NONE', 0, 'None',
     'Sentinel for instrument codes that are not curve instruments (options, credit, equity, and so on).',
     4, current_user, current_user, 'system.initial_load', 'Initial population of curve roles')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_curve_roles' as entity, count(*) as count
from ores_refdata_curve_roles_tbl;
