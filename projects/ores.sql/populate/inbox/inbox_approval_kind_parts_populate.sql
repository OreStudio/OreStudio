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
 * Approval Kind Parts Population Script
 *
 * The parts each kind of request names. An engine run settings change carries
 * one part for each number-changing group of settings, so a proposal names the
 * part of the group it would move and no other. The execution group changes no
 * numbers, so it names no part and a write to it lands directly.
 *
 * This script runs after the kinds and the parts, because it names both, and it
 * is idempotent.
 */

\echo '--- Approval Kind Parts ---'

insert into ores_inbox_approval_kind_parts_tbl (
    kind_code, tenant_id, part_code, version,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    ('reporting.engine_settings_change', ores_utility_system_tenant_id_fn(), 'fixings', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kind parts'),
    ('reporting.engine_settings_change', ores_utility_system_tenant_id_fn(), 'building', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kind parts')
on conflict (tenant_id, kind_code, part_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Approval Kind Parts' as entity, count(*) as count
from ores_inbox_approval_kind_parts_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
