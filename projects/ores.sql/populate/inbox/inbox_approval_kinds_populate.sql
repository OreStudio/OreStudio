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
 * Approval Kinds Population Script
 *
 * Seeds the kinds of approval request. Each kind names the permission a
 * decider needs, so this script runs after the IAM permissions. This script is
 * idempotent.
 *
 * A role request carries a review window, because an administrator who never
 * answers asks the member to wait forever. The inbox closes it when a decider
 * reaches for it, and again on its own sweep, so a queue nobody opens does not
 * rot either.
 *
 * An operational authorisation request does not expire: an unreleased trade is
 * a risk that a timeout must not hide. It allows a hold and asks for a comment
 * on every decision, as the operational authorisation queue does.
 */

\echo '--- Approval Kinds ---'

insert into ores_inbox_approval_kinds_tbl (
    tenant_id, code, version, name, description, decide_permission_code,
    allows_hold, comment_on_approve, expires_after_days, apply_workflow_type,
    apply_window_hours, approvals_required, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'iam.role_grant', 0, 'Role request',
     'A person asks to be given a role', 'iam::roles:assign',
     false, false, 14, null, null, 1, 10,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds'),
    (ores_utility_system_tenant_id_fn(), 'refdata.book_change', 0, 'Book change',
     'A person proposes changes to books', 'inbox::approvals:decide_operations',
     false, false, 14, 'book_apply_workflow', 24, 1, 20,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds'),
    (ores_utility_system_tenant_id_fn(), 'refdata.counterparty_change', 0, 'Counterparty change',
     'A person proposes changes to counterparties, their identifiers and the parties that may trade with them',
     'inbox::approvals:decide_operations',
     false, false, 14, 'counterparty_apply_workflow', 24, 1, 30,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds'),
    (ores_utility_system_tenant_id_fn(), 'trading.operational_authorisation', 0, 'Operational authorisation',
     'Operations is asked to release a live trade',
     'inbox::approvals:decide_operations',
     true, true, null, null, null, 1, 40,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
