/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software: redistribute under GPLv3 or later.
 *
 */

-- =============================================================================
-- Row-Level Security Policies for Workspace Tables
-- =============================================================================
-- Workspaces are private to the owning party within a tenant. The Live
-- workspace (sentinel UUID) is owned by the system party but must be readable
-- by all users in the tenant, so the party isolation policy explicitly exempts
-- it by UUID.

-- -----------------------------------------------------------------------------
-- Workspaces
-- -----------------------------------------------------------------------------
alter table ores_workspaces_tbl enable row level security;

-- Tenant isolation: a session can only see workspaces in its own tenant.
create policy workspaces_tenant_isolation_policy on ores_workspaces_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation (restrictive SELECT): a session sees only workspaces owned
-- by a party in its visible-party set, plus the Live workspace which belongs
-- to the system party but is readable by every party in the tenant.
create policy workspaces_party_isolation_policy on ores_workspaces_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
    OR id = ores_utility_live_workspace_id_fn()
);
