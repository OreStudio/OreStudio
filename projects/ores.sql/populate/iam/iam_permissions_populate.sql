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

set schema 'ores';

-- Account management permissions
select ores.upsert_permission('accounts:create', 'Create new user accounts');
select ores.upsert_permission('accounts:read', 'View user account details');
select ores.upsert_permission('accounts:update', 'Modify user account settings');
select ores.upsert_permission('accounts:delete', 'Delete user accounts');
select ores.upsert_permission('accounts:lock', 'Lock user accounts');
select ores.upsert_permission('accounts:unlock', 'Unlock user accounts');
select ores.upsert_permission('accounts:reset_password', 'Force password reset on user accounts');

-- Currency management permissions
select ores.upsert_permission('currencies:create', 'Create new currencies');
select ores.upsert_permission('currencies:read', 'View currency details');
select ores.upsert_permission('currencies:update', 'Modify currency settings');
select ores.upsert_permission('currencies:delete', 'Delete currencies');
select ores.upsert_permission('currencies:history', 'View currency version history');

-- Feature flags permissions
select ores.upsert_permission('flags:create', 'Create new feature flags');
select ores.upsert_permission('flags:read', 'View feature flag status');
select ores.upsert_permission('flags:update', 'Modify feature flag settings');
select ores.upsert_permission('flags:delete', 'Delete feature flags');

-- Login info permissions (read-only audit data)
select ores.upsert_permission('login_info:read', 'View login history and info');

-- Role management permissions
select ores.upsert_permission('roles:create', 'Create new roles');
select ores.upsert_permission('roles:read', 'View role details');
select ores.upsert_permission('roles:update', 'Modify role permissions');
select ores.upsert_permission('roles:delete', 'Delete roles');
select ores.upsert_permission('roles:assign', 'Assign roles to accounts');
select ores.upsert_permission('roles:revoke', 'Revoke roles from accounts');

-- Wildcard permission (superuser)
select ores.upsert_permission('*', 'Full access to all operations');

-- Show summary
select count(*) as total_permissions from ores.iam_permissions_tbl
where valid_to = ores.utility_infinity_timestamp_fn();
