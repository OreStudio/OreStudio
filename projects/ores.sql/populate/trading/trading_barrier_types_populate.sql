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
 * Barrier Type Population Script
 *
 * Seeds the closed barrier set. The rows are the ORE barrierType
 * simple type under external/ore/xsd/instruments.xsd.
 *
 * This script is idempotent.
 */

\echo '--- Barrier Type ---'

insert into ores_trading_barrier_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('UpAndOut', ores_utility_system_tenant_id_fn(), 0, 'Knocks out when the upper level is breached',
     current_user, 'system.initial_load', 'Seed barrier_type'),
    ('UpAndIn', ores_utility_system_tenant_id_fn(), 0, 'Knocks in when the upper level is breached',
     current_user, 'system.initial_load', 'Seed barrier_type'),
    ('DownAndOut', ores_utility_system_tenant_id_fn(), 0, 'Knocks out when the lower level is breached',
     current_user, 'system.initial_load', 'Seed barrier_type'),
    ('DownAndIn', ores_utility_system_tenant_id_fn(), 0, 'Knocks in when the lower level is breached',
     current_user, 'system.initial_load', 'Seed barrier_type'),
    ('KnockIn', ores_utility_system_tenant_id_fn(), 0, 'Knocks in on the stated condition',
     current_user, 'system.initial_load', 'Seed barrier_type'),
    ('KnockOut', ores_utility_system_tenant_id_fn(), 0, 'Knocks out on the stated condition',
     current_user, 'system.initial_load', 'Seed barrier_type'),
    ('CumulatedProfitCap', ores_utility_system_tenant_id_fn(), 0, 'Caps the cumulated profit',
     current_user, 'system.initial_load', 'Seed barrier_type'),
    ('CumulatedProfitCapPoints', ores_utility_system_tenant_id_fn(), 0, 'Caps the cumulated profit in points',
     current_user, 'system.initial_load', 'Seed barrier_type'),
    ('FixingCap', ores_utility_system_tenant_id_fn(), 0, 'Caps a fixing',
     current_user, 'system.initial_load', 'Seed barrier_type'),
    ('FixingFloor', ores_utility_system_tenant_id_fn(), 0, 'Floors a fixing',
     current_user, 'system.initial_load', 'Seed barrier_type')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Barrier Type' as entity, count(*) as count
from ores_trading_barrier_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
