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
 * Account Types Population Script
 *
 * Seeds the database with account type definitions.
 * This script is idempotent.
 */

\echo '--- Account Types ---'

insert into ores_iam_account_types_tbl (
    tenant_id, type, version, name, description, display_order,
    modified_by, change_reason_code, change_commentary
) values
    (ores_iam_system_tenant_id_fn(), 'user', 0, 'User',
     'Human user account that can authenticate with username and password', 0,
     current_user, 'system.initial_load', 'Initial population of account types'),
    (ores_iam_system_tenant_id_fn(), 'service', 0, 'Service',
     'Service account for non-human processes and applications', 10,
     current_user, 'system.initial_load', 'Initial population of account types'),
    (ores_iam_system_tenant_id_fn(), 'algorithm', 0, 'Algorithm',
     'Account for automated algorithms and computational processes', 20,
     current_user, 'system.initial_load', 'Initial population of account types'),
    (ores_iam_system_tenant_id_fn(), 'llm', 0, 'LLM',
     'Account for Large Language Model agents and AI systems', 30,
     current_user, 'system.initial_load', 'Initial population of account types')
on conflict (tenant_id, type)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'Account Types' as entity, count(*) as count
from ores_iam_account_types_tbl;
