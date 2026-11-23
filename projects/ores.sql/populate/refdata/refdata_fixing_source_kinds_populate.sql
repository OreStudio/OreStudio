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
 * Fixing Source Kinds Population Script
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Fixing Source Kinds ---'

insert into ores_refdata_fixing_source_kinds_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'published', 0, 'Published',
     'An administrator publishes the value, such as WM/Reuters or a central bank.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of fixing source kinds'),
    (ores_utility_system_tenant_id_fn(), 'keyed', 0, 'Keyed',
     'The value is keyed from a screen page at the source''s time.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of fixing source kinds'),
    (ores_utility_system_tenant_id_fn(), 'derived', 0, 'Derived',
     'The value is computed from other market data, such as a curve or two fixings through a common currency.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of fixing source kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
