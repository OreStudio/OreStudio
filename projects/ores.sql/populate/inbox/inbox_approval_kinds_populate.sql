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
 *
 * A curve publish is decided by Market Risk, one person, inside a two-week
 * window: a candidate curve that nobody answers leaves the desk pricing from
 * the published one, which is the safe failure. It allows a hold, because the
 * approver's third answer is "not yet" and the journey asks for it, and it asks
 * for no comment on an approval, because a refusal already carries the one the
 * asker reads. It names no apply workflow: the trader applies an approved
 * request through marketdata.v1.ops.publish_curve, as the third journey draws it.
 *
 * A curve withdrawal is decided by Market Risk, as a publish is, because taking
 * a curve out of use is the same risk decision read backwards. It does not
 * expire: a withdrawal nobody answers leaves a wrong set in use, so the ask must
 * stay visible rather than close in silence. It names no apply workflow either:
 * the market data service applies it through marketdata.v1.ops.withdraw_curve.
 *
 * An engine run settings change is decided by the parts the kind names, one for
 * each number-changing group of settings, so the kind's own decide permission
 * reaches none of its deciders and is the Market Risk one because that function
 * owns the numbers. It names the apply workflow the reporting service registers,
 * because a proposal nobody applies leaves the version in force as it was and
 * the person who asked waiting.
 *
 * The maker and checker pair is per kind. Only the two market data kinds name a
 * raise permission, and it is the code the market data service's own operations
 * already check: marketdata::curve_bootstrap:publish. A curve withdrawal is
 * raised by the account that may publish, as the withdrawal journey states, so
 * it names the same code. Every other kind names none, which keeps the rule it
 * had: the raise is the owning component's to gate.
 */

\echo '--- Approval Kinds ---'

insert into ores_inbox_approval_kinds_tbl (
    tenant_id, code, version, name, description, decide_permission_code,
    raise_permission_code,
    allows_hold, comment_on_approve, expires_after_days, apply_workflow_type,
    apply_window_hours, approvals_required, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'iam.role_grant', 0, 'Role request',
     'A person asks to be given a role', 'iam::roles:assign',
     null,
     false, false, 14, null, null, 1, 10,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds'),
    (ores_utility_system_tenant_id_fn(), 'refdata.book_change', 0, 'Book change',
     'A person proposes changes to books', 'inbox::approvals:decide_operations',
     null,
     false, false, 14, 'book_apply_workflow', 24, 1, 20,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds'),
    (ores_utility_system_tenant_id_fn(), 'refdata.counterparty_change', 0, 'Counterparty change',
     'A person proposes changes to counterparties, their identifiers and the parties that may trade with them',
     'inbox::approvals:decide_operations',
     null,
     false, false, 14, 'counterparty_apply_workflow', 24, 1, 30,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds'),
    (ores_utility_system_tenant_id_fn(), 'marketdata.curve_publish', 0, 'Curve publish',
     'A trader asks for a candidate curve to become the one the desk prices from',
     'inbox::approvals:decide_market_risk',
     'marketdata::curve_bootstrap:publish',
     true, false, 14, null, null, 1, 50,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds'),
    (ores_utility_system_tenant_id_fn(), 'marketdata.curve_withdrawal', 0, 'Curve withdrawal',
     'A trader asks for a published curve to be taken out of use, with the reason kept against the act',
     'inbox::approvals:decide_market_risk',
     'marketdata::curve_bootstrap:publish',
     true, false, null, null, null, 1, 51,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds'),
    (ores_utility_system_tenant_id_fn(), 'trading.operational_authorisation', 0, 'Operational authorisation',
     'Operations is asked to release a live trade',
     'inbox::approvals:decide_operations',
     null,
     true, true, null, null, null, 1, 40,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds'),
    (ores_utility_system_tenant_id_fn(), 'reporting.engine_settings_change', 0, 'Engine run settings change',
     'A person proposes a change to a group of engine run settings that moves the numbers a run computes',
     'inbox::approvals:decide_market_risk',
     null,
     true, false, 14, 'engine_settings_apply_workflow', 24, 1, 60,
     current_user, current_user, 'system.initial_load', 'Initial population of approval kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
