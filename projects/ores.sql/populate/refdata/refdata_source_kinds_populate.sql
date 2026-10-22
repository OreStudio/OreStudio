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
 * Source Kinds Population Script
 *
 * Populates the valid market_data_source.kind values: live, replay, test,
 * simulated and derived. The values are the whole vocabulary; a new kind is a
 * new row here, not a code change.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Source Kinds ---'

insert into ores_refdata_source_kinds_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'live', 0, 'Live',
     'The adapter reads a real vendor session.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of source kinds'),
    (ores_utility_system_tenant_id_fn(), 'replay', 0, 'Replay',
     'The adapter replays recorded vendor ticks.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of source kinds'),
    (ores_utility_system_tenant_id_fn(), 'test', 0, 'Test',
     'The adapter is a vendor-supplied test environment or a test harness.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of source kinds'),
    (ores_utility_system_tenant_id_fn(), 'simulated', 0, 'Simulated',
     'The adapter generates values from generators.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of source kinds'),
    (ores_utility_system_tenant_id_fn(), 'derived', 0, 'Derived',
     'The market data service computes the values from other series, as a composite does.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of source kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_source_kinds' as entity, count(*) as count
from ores_refdata_source_kinds_tbl;
