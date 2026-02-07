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

-- Tenant infrastructure (must be first - other tables depend on tenants)
\ir ./iam_tenant_types_create.sql
\ir ./iam_tenant_statuses_create.sql
\ir ./iam_tenant_functions_create.sql
\ir ./iam_tenants_create.sql
\ir ./iam_tenants_notify_trigger_create.sql

-- Account types (must come before accounts)
\ir ./iam_account_types_create.sql

-- Accounts
\ir ./iam_accounts_create.sql
\ir ./iam_accounts_notify_trigger_create.sql
\ir ./iam_login_info_create.sql

-- Sessions
\ir ./iam_sessions_create.sql
\ir ./iam_session_stats_create.sql

-- RBAC
\ir ./iam_permissions_create.sql
\ir ./iam_permissions_notify_trigger_create.sql
\ir ./iam_roles_create.sql
\ir ./iam_roles_notify_trigger_create.sql
\ir ./iam_role_permissions_create.sql
\ir ./iam_account_roles_create.sql
\ir ./iam_rbac_functions_create.sql

-- Account-party association (depends on accounts and refdata.parties)
\ir ./iam_account_party_create.sql

-- Tenant lifecycle (must come after all tables it references)
\ir ./iam_tenant_provisioner_create.sql
\ir ./iam_tenant_deprovisioner_create.sql
\ir ./iam_tenant_terminator_create.sql

-- Admin utilities (helper functions for administrative tasks)
\ir ./iam_admin_utilities_create.sql
