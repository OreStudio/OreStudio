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
 * Tenor Kinds Population Script
 *
 * Populates the valid tenor kind values (PERIOD, SPECIAL).
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Tenor Kinds ---'

insert into ores_refdata_tenor_kinds_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'PERIOD', 0, 'Period',
     'Regular nD/nW/nM/nY duration with a fixed offset from the convention''s anchor.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of tenor kinds'),
    (ores_utility_system_tenant_id_fn(), 'SPECIAL', 0, 'Special',
     'Label with no fixed offset (e.g. O/N, SPOT, TODAY), resolved by rule per convention.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of tenor kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_tenor_kinds' as entity, count(*) as count
from ores_refdata_tenor_kinds_tbl;
