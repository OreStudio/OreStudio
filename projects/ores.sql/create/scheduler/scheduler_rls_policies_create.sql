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
-- Row-Level Security Policies for Scheduler Tables
-- =============================================================================
-- Scheduler jobs are tenant-scoped: visibility is by tenant only.  Party
-- isolation is NOT applied here because scheduler jobs are system-level
-- operations (e.g. report reconciliation) created by service accounts whose
-- party may differ from the end-user's party.  Enforcing party isolation would
-- make jobs invisible to legitimate users in the same tenant.
--
-- Note: pg_cron itself (cron.job, cron.job_run_details) runs as the database
-- owner and ignores RLS. Tenant isolation for job instances is enforced
-- at the application layer by filtering on the cron_job_id values registered
-- in ores_scheduler_job_definitions_tbl.

-- -----------------------------------------------------------------------------
-- Job Definitions
-- -----------------------------------------------------------------------------
alter table ores_scheduler_job_definitions_tbl enable row level security;

-- Tenant isolation: all operations restricted to current tenant.
-- Exception: the system service context (tenant = system sentinel) is allowed
-- to read and write jobs for any tenant during cross-tenant operations such as
-- startup reconciliation.  The INSERT trigger always validates tenant_id FK.
create policy ores_scheduler_job_definitions_tenant_isolation_policy
on ores_scheduler_job_definitions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_iam_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_iam_system_tenant_id_fn()
);
