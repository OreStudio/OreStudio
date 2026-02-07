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
 * Party Statuses Population Script
 *
 * Populates lifecycle states for party and counterparty records.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Party Statuses ---'

insert into ores_refdata_party_statuses_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'Active', 0, 'Active',
     'The party is currently active and available for use in transactions and reporting.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of party statuses'),
    (ores_iam_system_tenant_id_fn(), 'Inactive', 0, 'Inactive',
     'The party is no longer active. Existing references are preserved but no new transactions should reference this party.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of party statuses'),
    (ores_iam_system_tenant_id_fn(), 'Suspended', 0, 'Suspended',
     'The party is temporarily suspended pending review or investigation. May be reactivated.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of party statuses')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_party_statuses' as entity, count(*) as count
from ores_refdata_party_statuses_tbl;
