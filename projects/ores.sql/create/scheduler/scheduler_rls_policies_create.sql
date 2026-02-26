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
-- Scheduler jobs are party-scoped: each job belongs to exactly one party within
-- a tenant. RLS enforces both tenant and party isolation.
--
-- Note: pg_cron itself (cron.job, cron.job_run_details) runs as the database
-- owner and ignores RLS. Tenant/party isolation for job instances is enforced
-- at the application layer by filtering on the cron_job_id values registered
-- in ores_scheduler_job_definitions_tbl.

-- -----------------------------------------------------------------------------
-- Job Definitions
-- -----------------------------------------------------------------------------
alter table ores_scheduler_job_definitions_tbl enable row level security;

-- Tenant isolation: all operations restricted to current tenant.
create policy ores_scheduler_job_definitions_tenant_isolation_policy
on ores_scheduler_job_definitions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement — no party context means no rows visible.
-- FOR SELECT only: the trigger validates party_id FK on INSERT/UPDATE, so
-- WITH CHECK is not needed and would block bulk operations.
create policy ores_scheduler_job_definitions_party_isolation_policy
on ores_scheduler_job_definitions_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);
