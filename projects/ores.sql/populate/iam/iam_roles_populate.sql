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

-- Create roles
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Admin', 'Full administrative access to all system functions');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'Trading operations - currency read access');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Sales', 'Sales operations - read-only currency access');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'Operations - currency management and account viewing');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Support', 'Support - read-only access to all resources and admin screens');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Viewer', 'Viewer - basic read-only access to domain data');

-- Platform-level roles (system tenant only)
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'Platform super administrator with tenant management access');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'TenantAdmin', 'Tenant administrator with full access within a tenant');

-- Assign permissions to Admin role (wildcard)
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Admin', '*');

-- Assign permissions to SuperAdmin role (platform-level)
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', '*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'tenants:create');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'tenants:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'tenants:update');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'tenants:suspend');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'tenants:terminate');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'tenants:impersonate');

-- Assign permissions to TenantAdmin role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'TenantAdmin', '*');

-- Assign permissions to Trading role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'currencies:history');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'flags:read');

-- Assign permissions to Sales role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Sales', 'currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Sales', 'flags:read');

-- Assign permissions to Operations role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'currencies:create');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'currencies:update');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'currencies:delete');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'currencies:history');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'flags:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'accounts:read');

-- Assign permissions to Support role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'accounts:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'currencies:history');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'flags:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'login_info:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'roles:read');

-- Assign permissions to Viewer role (default for new accounts)
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Viewer', 'currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Viewer', 'flags:read');

-- Show summary
select 'Roles:' as summary, count(*) as count from ores_iam_roles_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Role-Permission assignments:', count(*) from ores_iam_role_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
