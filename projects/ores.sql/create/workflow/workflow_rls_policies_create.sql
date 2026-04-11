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
-- Row-Level Security Policies for Workflow Tables
-- =============================================================================
-- Workflow instances are strictly tenant-scoped; only the owning tenant may
-- read or write its own instances. Steps inherit tenant isolation via their
-- parent instance.

-- -----------------------------------------------------------------------------
-- Workflow Instances
-- -----------------------------------------------------------------------------
-- The workflow engine runs with system-tenant context and must be able to
-- create and manage instances for any tenant (cross-tenant service).
-- Regular callers are restricted to their own tenant's instances.
alter table ores_workflow_workflow_instances_tbl enable row level security;

create policy ores_workflow_workflow_instances_tenant_isolation_policy
on ores_workflow_workflow_instances_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_iam_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_iam_system_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Workflow Steps (tenant isolation via parent instance join)
-- -----------------------------------------------------------------------------
-- System-tenant context bypasses the join check to allow the workflow engine
-- to read and write steps for any tenant.
alter table ores_workflow_workflow_steps_tbl enable row level security;

create policy ores_workflow_workflow_steps_tenant_isolation_policy
on ores_workflow_workflow_steps_tbl
for all using (
    ores_iam_current_tenant_id_fn() = ores_iam_system_tenant_id_fn()
    OR exists (
        select 1
        from ores_workflow_workflow_instances_tbl i
        where i.id = workflow_id
          and i.tenant_id = ores_iam_current_tenant_id_fn()
    )
);
