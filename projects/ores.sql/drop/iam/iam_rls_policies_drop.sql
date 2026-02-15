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

-- =============================================================================
-- Drop Row-Level Security Policies for IAM Tables
-- =============================================================================
-- Must be dropped before the corresponding tables are dropped.

-- Account Parties
drop policy if exists ores_iam_account_parties_tenant_isolation_policy on "ores_iam_account_parties_tbl";

-- Tenants
drop policy if exists ores_iam_tenants_write_policy on "ores_iam_tenants_tbl";
drop policy if exists ores_iam_tenants_read_policy on "ores_iam_tenants_tbl";

-- Login Info
drop policy if exists ores_iam_login_info_tenant_isolation_policy on "ores_iam_login_info_tbl";

-- Sessions
drop policy if exists ores_iam_sessions_tenant_isolation_policy on "ores_iam_sessions_tbl";

-- Role Permissions
drop policy if exists ores_iam_role_permissions_tenant_isolation_policy on "ores_iam_role_permissions_tbl";

-- Account Roles
drop policy if exists ores_iam_account_roles_tenant_isolation_policy on "ores_iam_account_roles_tbl";

-- Permissions
drop policy if exists ores_iam_permissions_tenant_isolation_policy on "ores_iam_permissions_tbl";

-- Roles
drop policy if exists ores_iam_roles_tenant_isolation_policy on "ores_iam_roles_tbl";

-- Accounts
drop policy if exists ores_iam_accounts_tenant_isolation_policy on "ores_iam_accounts_tbl";
