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
 * Calibration Fallbacks Population Script
 *
 * Seeds the two answers a calibration policy may give when no candidate has a
 * usable fixing: keep the feed's fixed level, or refuse to start it.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Calibration Fallbacks ---'

insert into ores_synthetic_calibration_fallbacks_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'keep_fixed', 0, 'Keep the fixed level',
     'Start the feed from its configured fixed level and record that it took no published value.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of calibration fallbacks'),
    (ores_utility_system_tenant_id_fn(), 'refuse', 0, 'Refuse to start',
     'Do not start the feed when no candidate has a usable fixing, and record the refusal.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of calibration fallbacks')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'synthetic_calibration_fallbacks' as entity, count(*) as count
from ores_synthetic_calibration_fallbacks_tbl;
