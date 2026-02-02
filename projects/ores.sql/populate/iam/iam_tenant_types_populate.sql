/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
 * Tenant Types Population Script
 *
 * Seeds the database with tenant type definitions.
 * This script is idempotent.
 */

\echo '--- Tenant Types ---'

insert into ores_iam_tenant_types_tbl (
    tenant_id, type, version, name, description, display_order,
    modified_by, change_reason_code, change_commentary
) values
    (ores_iam_system_tenant_id_fn(), 'platform', 0, 'Platform',
     'System-level tenant for platform administration and shared governance data', 0,
     current_user, 'system.initial_load', 'Initial population of tenant types'),
    (ores_iam_system_tenant_id_fn(), 'organisation', 0, 'Organisation',
     'Customer organisation tenant for isolated business operations', 10,
     current_user, 'system.initial_load', 'Initial population of tenant types')
on conflict (tenant_id, type)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'Tenant Types' as entity, count(*) as count
from ores_iam_tenant_types_tbl;
