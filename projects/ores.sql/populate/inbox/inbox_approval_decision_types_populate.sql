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
 * Approval Decision Types Population Script
 *
 * Seeds the decisions a person can take on an approval request. This script is
 * idempotent.
 */

\echo '--- Approval Decision Types ---'

insert into ores_inbox_approval_decision_types_tbl (
    tenant_id, code, version, name, description, requires_comment, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'approve', 0, 'Approve',
     'Decide yes', false, 10,
     current_user, current_user, 'system.initial_load', 'Initial population of approval decision types'),
    (ores_utility_system_tenant_id_fn(), 'refuse', 0, 'Refuse',
     'Decide no; a refusal is final', true, 20,
     current_user, current_user, 'system.initial_load', 'Initial population of approval decision types'),
    (ores_utility_system_tenant_id_fn(), 'hold', 0, 'Hold',
     'Park the request pending a question', true, 30,
     current_user, current_user, 'system.initial_load', 'Initial population of approval decision types'),
    (ores_utility_system_tenant_id_fn(), 'resume', 0, 'Resume',
     'Return a held request to waiting', false, 40,
     current_user, current_user, 'system.initial_load', 'Initial population of approval decision types'),
    (ores_utility_system_tenant_id_fn(), 'withdraw', 0, 'Withdraw',
     'The person who asked takes the request back', false, 50,
     current_user, current_user, 'system.initial_load', 'Initial population of approval decision types'),
    (ores_utility_system_tenant_id_fn(), 'reverse', 0, 'Reverse',
     'Undo an approval; the owning component undoes what it applied', true, 60,
     current_user, current_user, 'system.initial_load', 'Initial population of approval decision types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
