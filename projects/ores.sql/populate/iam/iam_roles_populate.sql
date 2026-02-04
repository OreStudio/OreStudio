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
 * - SuperAdmin: Platform super administrator (tenant0) with tenant management
 * - TenantAdmin: Tenant administrator with full access within a tenant
 * - Trading: Currency read access for trading operations
 * - Sales: Read-only currency access for sales
 * - Operations: Currency management and account viewing
 * - Support: Read-only access to all resources
 * - Viewer: Basic read-only access (default role for new accounts)
 *
 * Prerequisites:
 * - permissions_populate.sql must be run first
 */

-- Create platform-level admin roles
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'Platform super administrator with tenant management access');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'TenantAdmin', 'Tenant administrator with full access within a tenant');

-- Create functional roles
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'Trading operations - currency read access');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Sales', 'Sales operations - read-only currency access');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'Operations - currency management and account viewing');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Support', 'Support - read-only access to all resources and admin screens');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Viewer', 'Viewer - basic read-only access to domain data');

-- Assign permissions to SuperAdmin role (platform-level)
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', '*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:create');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:update');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:suspend');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:terminate');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:impersonate');

-- Assign permissions to TenantAdmin role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'TenantAdmin', '*');

-- Assign permissions to Trading role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'refdata::currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'refdata::currencies:history');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'variability::flags:read');

-- Assign permissions to Sales role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Sales', 'refdata::currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Sales', 'variability::flags:read');

-- Assign permissions to Operations role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'refdata::currencies:create');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'refdata::currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'refdata::currencies:update');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'refdata::currencies:delete');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'refdata::currencies:history');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'variability::flags:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'iam::accounts:read');

-- Assign permissions to Support role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'iam::accounts:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'refdata::currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'refdata::currencies:history');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'variability::flags:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'iam::login_info:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'iam::roles:read');

-- Assign permissions to Viewer role (default for new accounts)
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Viewer', 'refdata::currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Viewer', 'variability::flags:read');

-- Show summary
select 'Roles:' as summary, count(*) as count from ores_iam_roles_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Role-Permission assignments:', count(*) from ores_iam_role_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
