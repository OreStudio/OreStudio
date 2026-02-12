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
 * Party Categories Population Script
 *
 * Populates structural classifications for parties:
 * - System: Auto-created with the tenant for platform administration.
 * - Operational: Business entities that participate in financial transactions.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Party Categories ---'

insert into ores_refdata_party_categories_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'System', 0, 'System',
     'Auto-created with the tenant for platform administration. Every tenant has exactly one system party.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of party categories'),
    (ores_iam_system_tenant_id_fn(), 'Operational', 0, 'Operational',
     'Business entities that participate in financial transactions. Created during normal system operation.',
     10, current_user, current_user, 'system.initial_load', 'Initial population of party categories')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_party_categories' as entity, count(*) as count
from ores_refdata_party_categories_tbl;
