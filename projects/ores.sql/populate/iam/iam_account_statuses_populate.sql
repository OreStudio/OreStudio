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
 * Account Statuses Population Script
 *
 * Seeds the database with account lifecycle status definitions.
 * This script is idempotent.
 */

\echo '--- Account Statuses ---'

insert into ores_iam_account_statuses_tbl (
    tenant_id, status, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'active', 0, 'Active',
     'Account is active and can sign in', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of account statuses'),
    (ores_utility_system_tenant_id_fn(), 'pending', 0, 'Pending',
     'Account exists but an administrator has not finished setting it up', 10,
     current_user, current_user, 'system.initial_load', 'Initial population of account statuses')
on conflict (tenant_id, status)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'Account Statuses' as entity, count(*) as count
from ores_iam_account_statuses_tbl;
