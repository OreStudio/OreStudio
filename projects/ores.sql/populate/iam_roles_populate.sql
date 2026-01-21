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
 * Roles Population Script
 *
 * Seeds the RBAC roles table with predefined roles and their permission
 * assignments. This script is idempotent - running it multiple times will
 * not create duplicate entries.
 *
 * Predefined Roles:
 * - Admin: Full access (wildcard permission)
 * - Trading: Currency read access for trading operations
 * - Sales: Read-only currency access for sales
 * - Operations: Currency management and account viewing
 * - Support: Read-only access to all resources
 * - Viewer: Basic read-only access (default role for new accounts)
 *
 * Prerequisites:
 * - permissions_populate.sql must be run first
 */

set schema 'ores';

-- Create roles
select ores.upsert_role('Admin', 'Full administrative access to all system functions');
select ores.upsert_role('Trading', 'Trading operations - currency read access');
select ores.upsert_role('Sales', 'Sales operations - read-only currency access');
select ores.upsert_role('Operations', 'Operations - currency management and account viewing');
select ores.upsert_role('Support', 'Support - read-only access to all resources and admin screens');
select ores.upsert_role('Viewer', 'Viewer - basic read-only access to domain data');

-- Assign permissions to Admin role (wildcard)
select ores.assign_permission_to_role('Admin', '*');

-- Assign permissions to Trading role
select ores.assign_permission_to_role('Trading', 'currencies:read');
select ores.assign_permission_to_role('Trading', 'currencies:history');
select ores.assign_permission_to_role('Trading', 'flags:read');

-- Assign permissions to Sales role
select ores.assign_permission_to_role('Sales', 'currencies:read');
select ores.assign_permission_to_role('Sales', 'flags:read');

-- Assign permissions to Operations role
select ores.assign_permission_to_role('Operations', 'currencies:create');
select ores.assign_permission_to_role('Operations', 'currencies:read');
select ores.assign_permission_to_role('Operations', 'currencies:update');
select ores.assign_permission_to_role('Operations', 'currencies:delete');
select ores.assign_permission_to_role('Operations', 'currencies:history');
select ores.assign_permission_to_role('Operations', 'flags:read');
select ores.assign_permission_to_role('Operations', 'accounts:read');

-- Assign permissions to Support role
select ores.assign_permission_to_role('Support', 'accounts:read');
select ores.assign_permission_to_role('Support', 'currencies:read');
select ores.assign_permission_to_role('Support', 'currencies:history');
select ores.assign_permission_to_role('Support', 'flags:read');
select ores.assign_permission_to_role('Support', 'login_info:read');
select ores.assign_permission_to_role('Support', 'roles:read');

-- Assign permissions to Viewer role (default for new accounts)
select ores.assign_permission_to_role('Viewer', 'currencies:read');
select ores.assign_permission_to_role('Viewer', 'flags:read');

-- Show summary
select 'Roles:' as summary, count(*) as count from ores.iam_roles_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Role-Permission assignments:', count(*) from ores.iam_role_permissions_tbl
where valid_to = ores.utility_infinity_timestamp_fn();
