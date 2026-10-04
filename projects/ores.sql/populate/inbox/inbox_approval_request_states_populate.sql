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
 * Approval Request States Population Script
 *
 * Seeds the states of the approval request lifecycle. This script is
 * idempotent.
 */

\echo '--- Approval Request States ---'

insert into ores_inbox_approval_request_states_tbl (
    tenant_id, code, version, name, description, is_final, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'waiting', 0, 'Waiting',
     'Asked, and not yet decided', false, 10,
     current_user, current_user, 'system.initial_load', 'Initial population of approval request states'),
    (ores_utility_system_tenant_id_fn(), 'held', 0, 'Held',
     'Parked pending a question; a suspension, not a refusal', false, 20,
     current_user, current_user, 'system.initial_load', 'Initial population of approval request states'),
    (ores_utility_system_tenant_id_fn(), 'approved', 0, 'Approved',
     'Decided yes; the owning component has applied it', true, 30,
     current_user, current_user, 'system.initial_load', 'Initial population of approval request states'),
    (ores_utility_system_tenant_id_fn(), 'refused', 0, 'Refused',
     'Decided no', true, 40,
     current_user, current_user, 'system.initial_load', 'Initial population of approval request states'),
    (ores_utility_system_tenant_id_fn(), 'withdrawn', 0, 'Withdrawn',
     'The person who asked took it back', true, 50,
     current_user, current_user, 'system.initial_load', 'Initial population of approval request states'),
    (ores_utility_system_tenant_id_fn(), 'expired', 0, 'Expired',
     'Nobody decided before it expired', true, 60,
     current_user, current_user, 'system.initial_load', 'Initial population of approval request states')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
