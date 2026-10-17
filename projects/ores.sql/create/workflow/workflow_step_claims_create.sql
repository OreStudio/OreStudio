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

/**
 * Workflow Step Claims Table
 *
 * One row for each step whose work is in flight. A step command is delivered at
 * least once, so two deliveries for one step can arrive at once; this row is
 * what makes exactly one of them do the work. The row holds who holds the step
 * -- the service running the work, as it names itself -- when the lease it holds
 * ends, and how many times the step has been claimed.
 *
 * The table is a lease rather than a record. A step's own state is what refuses
 * a replay after the work is done, so the row is removed the moment the step
 * leaves in_progress, and what remains is only the claims that are live now.
 *
 * A step belongs to a tenant and no policy lets the system tenant read another
 * tenant's rows, so a claim any tenant's step can take cannot be a query over
 * the steps of all of them. The claim belongs to the installation instead. It
 * holds an id, a time and a count and no business data of a tenant. The service
 * reads and writes it as the system tenant.
 *
 * The table is hand-written, not generated. It is a mutable current-state table
 * with atomic claim statements, and the policy below admits only a session of the
 * system tenant, a shape the entity templates do not offer.
 */

create table if not exists "ores_workflow_step_claims_tbl" (
    "step_id" uuid not null,
    "tenant_id" uuid not null default ores_utility_system_tenant_id_fn(),
    "work_tenant_id" uuid not null,
    "owner" text not null,
    "claimed_at" timestamp with time zone not null default now(),
    "lease_expires_at" timestamp with time zone not null,
    "attempts" integer not null default 0,
    primary key (step_id)
);

-- The leases that have ended, for the sweep that reads them.
create index if not exists workflow_step_claims_lease_idx
on "ores_workflow_step_claims_tbl" (lease_expires_at);

-- =============================================================================
-- Row-level security: the claim belongs to the installation
-- Only a session of the system tenant reads or writes it, and only the system
-- tenant's own rows. No other tenant reads any claim, and an operator who has
-- entered a tenant reads none of it. The policy is written as an own-tenant
-- comparison that is also the system tenant, so it admits no tenant's rows to
-- anyone else and is not a widening.
-- =============================================================================
-- Both comparisons are needed. The first admits the session's own tenant, which
-- is every table's rule. The second admits only the system tenant, which is what
-- makes the claim the installation's: a tenant's session passes the first test
-- for its own rows and fails the second.
alter table ores_workflow_step_claims_tbl enable row level security;

drop policy if exists workflow_step_claims_installation_policy
    on ores_workflow_step_claims_tbl;

create policy workflow_step_claims_installation_policy
on ores_workflow_step_claims_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    and tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    and tenant_id = ores_utility_system_tenant_id_fn()
);
