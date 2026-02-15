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
 * Book Statuses Population Script
 *
 * Populates the lifecycle states for book records.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Book Statuses ---'

insert into ores_refdata_book_statuses_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'Active', 0, 'Active',
     'Book is open and accepting new trades.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of book statuses'),
    (ores_iam_system_tenant_id_fn(), 'Closed', 0, 'Closed',
     'Book is closed. Existing trades remain but no new trades are allowed.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of book statuses'),
    (ores_iam_system_tenant_id_fn(), 'Frozen', 0, 'Frozen',
     'Book is frozen. No modifications allowed including new trades or amendments.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of book statuses')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_book_statuses' as entity, count(*) as count
from ores_refdata_book_statuses_tbl;
