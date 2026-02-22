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

-- =============================================================================
-- Row-Level Security Policies for IAM Tables
-- =============================================================================
-- These policies enforce strict tenant isolation for IAM data.
-- Each tenant can only see and modify their own accounts, roles, permissions,
-- etc. All tenants are fully isolated, including the system tenant.

-- -----------------------------------------------------------------------------
-- Accounts
-- -----------------------------------------------------------------------------
alter table ores_iam_accounts_tbl enable row level security;

create policy ores_iam_accounts_tenant_isolation_policy on ores_iam_accounts_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Roles
-- -----------------------------------------------------------------------------
alter table ores_iam_roles_tbl enable row level security;

create policy ores_iam_roles_tenant_isolation_policy on ores_iam_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Permissions
-- -----------------------------------------------------------------------------
alter table ores_iam_permissions_tbl enable row level security;

create policy ores_iam_permissions_tenant_isolation_policy on ores_iam_permissions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Account Roles (many-to-many)
-- -----------------------------------------------------------------------------
alter table ores_iam_account_roles_tbl enable row level security;

create policy ores_iam_account_roles_tenant_isolation_policy on ores_iam_account_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Role Permissions (many-to-many)
-- -----------------------------------------------------------------------------
alter table ores_iam_role_permissions_tbl enable row level security;

create policy ores_iam_role_permissions_tenant_isolation_policy on ores_iam_role_permissions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Sessions
-- -----------------------------------------------------------------------------
alter table ores_iam_sessions_tbl enable row level security;

create policy ores_iam_sessions_tenant_isolation_policy on ores_iam_sessions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Login Info
-- -----------------------------------------------------------------------------
alter table ores_iam_login_info_tbl enable row level security;

create policy ores_iam_login_info_tenant_isolation_policy on ores_iam_login_info_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenants Table
-- -----------------------------------------------------------------------------
-- All tenant records are owned by system tenant (tenant_id = system_tenant_id).
-- The system tenant can manage all tenants (create, update, delete).
-- Regular tenants can only read their own tenant record (by id).
alter table ores_iam_tenants_tbl enable row level security;

create policy ores_iam_tenants_read_policy on ores_iam_tenants_tbl
for select using (
    id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_iam_tenants_write_policy on ores_iam_tenants_tbl
for all using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Session Samples
-- -----------------------------------------------------------------------------
alter table ores_iam_session_samples_tbl enable row level security;

create policy ores_iam_session_samples_tenant_isolation_policy on ores_iam_session_samples_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Account Parties (many-to-many)
-- -----------------------------------------------------------------------------
alter table ores_iam_account_parties_tbl enable row level security;

create policy ores_iam_account_parties_tenant_isolation_policy on ores_iam_account_parties_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
