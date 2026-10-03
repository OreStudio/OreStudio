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
 * Shift Types Population Script
 *
 * Seeds how an ORE shift is applied: the shiftType enumeration in
 * external/ore/xsd/ore_types.xsd.
 * This script is idempotent.
 */

\echo '--- Shift Types ---'

insert into ores_analytics_shift_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Relative', ores_utility_system_tenant_id_fn(), 0, 'The shift is a proportion of the value.',
     'ores_analytics_service', 'system.initial_load', 'Seed shift types'),
    ('Absolute', ores_utility_system_tenant_id_fn(), 0, 'The shift is added to the value.',
     'ores_analytics_service', 'system.initial_load', 'Seed shift types'),
    ('EqualTo', ores_utility_system_tenant_id_fn(), 0, 'The value is replaced by the shift.',
     'ores_analytics_service', 'system.initial_load', 'Seed shift types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
