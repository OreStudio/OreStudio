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
 * Approval Parts Population Script
 *
 * Seeds the functions whose approval a gated change can need. Each part names
 * the permission its decider holds. The controller answers first. The other
 * parts answer together. This script runs after the IAM permissions and is
 * idempotent.
 */

\echo '--- Approval Parts ---'

insert into ores_inbox_approval_parts_tbl (
    tenant_id, code, version, name, description, decide_permission_code,
    answer_order, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'controller', 0, 'Controller',
     'The first independent check on a new book', 'inbox::approvals:decide_controller',
     1, 10,
     current_user, current_user, 'system.initial_load', 'Initial population of approval parts'),
    (ores_utility_system_tenant_id_fn(), 'finance', 0, 'Finance',
     'Accounting identity, the ledger feed and closing a book', 'inbox::approvals:decide_finance',
     2, 20,
     current_user, current_user, 'system.initial_load', 'Initial population of approval parts'),
    (ores_utility_system_tenant_id_fn(), 'market_risk', 0, 'Market Risk',
     'Classification, capital treatment, agreements and conventions', 'inbox::approvals:decide_market_risk',
     2, 30,
     current_user, current_user, 'system.initial_load', 'Initial population of approval parts'),
    (ores_utility_system_tenant_id_fn(), 'operations', 0, 'Operations',
     'Access, memberships, links, allowed currencies and products', 'inbox::approvals:decide_operations',
     2, 40,
     current_user, current_user, 'system.initial_load', 'Initial population of approval parts')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
