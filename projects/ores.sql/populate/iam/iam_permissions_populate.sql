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
 * Permission naming convention: "component::resource:action"
 * - component: The system component (iam, refdata, variability, etc.)
 * - resource: The entity type (accounts, currencies, etc.)
 * - action: The operation (create, read, update, delete, etc.)
 *
 * Wildcard permissions:
 * - "*" grants all permissions (superuser)
 * - "component::*" grants all permissions within a component
 */

-- =============================================================================
-- IAM Component Permissions
-- =============================================================================

-- Account management permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:create', 'Create new user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:read', 'View user account details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:update', 'Modify user account settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:delete', 'Delete user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:lock', 'Lock user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:unlock', 'Unlock user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:reset_password', 'Force password reset on user accounts');

-- Role management permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:create', 'Create new roles');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:read', 'View role details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:update', 'Modify role permissions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:delete', 'Delete roles');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:assign', 'Assign roles to accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:revoke', 'Revoke roles from accounts');

-- Tenant management permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:create', 'Create new tenants');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:read', 'View tenant details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:update', 'Modify tenant settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:suspend', 'Suspend tenants');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:terminate', 'Terminate tenants');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:impersonate', 'Access other tenants');

-- Login info permissions (read-only audit data)
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::login_info:read', 'View login history and info');

-- IAM component wildcard
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::*', 'Full access to all IAM operations');

-- =============================================================================
-- Reference Data Component Permissions
-- =============================================================================

-- Currency management permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currencies:create', 'Create new currencies');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currencies:read', 'View currency details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currencies:update', 'Modify currency settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currencies:delete', 'Delete currencies');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currencies:history', 'View currency version history');

-- Refdata component wildcard
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::*', 'Full access to all reference data operations');

-- =============================================================================
-- Variability Component Permissions
-- =============================================================================

-- Feature flags permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'variability::flags:create', 'Create new feature flags');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'variability::flags:read', 'View feature flag status');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'variability::flags:update', 'Modify feature flag settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'variability::flags:delete', 'Delete feature flags');

-- Variability component wildcard
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'variability::*', 'Full access to all variability operations');

-- =============================================================================
-- Global Wildcard Permission
-- =============================================================================

-- Wildcard permission (superuser)
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), '*', 'Full access to all operations');

-- Show summary
select count(*) as total_permissions from ores_iam_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
