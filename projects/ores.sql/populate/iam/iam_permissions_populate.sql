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
 * Permissions Population Script
 *
 * Seeds the RBAC permissions table with predefined permission codes.
 * This script is idempotent - running it multiple times will not create
 * duplicate entries.
 *
 * Permission naming convention: "resource:action"
 * Special permission: "*" grants all permissions (superuser)
 */

-- Account management permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'accounts:create', 'Create new user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'accounts:read', 'View user account details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'accounts:update', 'Modify user account settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'accounts:delete', 'Delete user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'accounts:lock', 'Lock user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'accounts:unlock', 'Unlock user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'accounts:reset_password', 'Force password reset on user accounts');

-- Currency management permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'currencies:create', 'Create new currencies');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'currencies:read', 'View currency details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'currencies:update', 'Modify currency settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'currencies:delete', 'Delete currencies');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'currencies:history', 'View currency version history');

-- Feature flags permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'flags:create', 'Create new feature flags');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'flags:read', 'View feature flag status');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'flags:update', 'Modify feature flag settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'flags:delete', 'Delete feature flags');

-- Login info permissions (read-only audit data)
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'login_info:read', 'View login history and info');

-- Role management permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'roles:create', 'Create new roles');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'roles:read', 'View role details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'roles:update', 'Modify role permissions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'roles:delete', 'Delete roles');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'roles:assign', 'Assign roles to accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'roles:revoke', 'Revoke roles from accounts');

-- Tenant management permissions (platform tenant only)
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'tenants:create', 'Create new tenants');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'tenants:read', 'View tenant details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'tenants:update', 'Modify tenant settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'tenants:suspend', 'Suspend tenants');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'tenants:terminate', 'Terminate tenants');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'tenants:impersonate', 'Access other tenants');

-- Wildcard permission (superuser)
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), '*', 'Full access to all operations');

-- Show summary
select count(*) as total_permissions from ores_iam_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
