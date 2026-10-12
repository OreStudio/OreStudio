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
 * Work Ledger Table
 *
 * The queue of the compute grid. One entry for each result that waits or runs,
 * holding what the service needs to choose a job and to find a lease that has
 * ended: the tenant that owns the result, the platforms that can run it, the
 * priority of its workunit, the host that holds it, the lease and the number of
 * attempts.
 *
 * A result belongs to a tenant and no policy lets the system tenant read another
 * tenant's rows, so a queue that serves every tenant cannot be a query over the
 * results of all of them. The ledger belongs to the installation instead. It
 * holds ids, times and counts and no business data of a tenant. The service
 * reads and writes it as the system tenant, and enters a tenant only to write
 * that tenant's own result.
 *
 * The table is hand-written, not generated. It is a mutable current-state table
 * with atomic claim statements, and the policy below admits only a session of the
 * system tenant, a shape the entity templates do not offer.
 */

create table if not exists "ores_compute_work_ledger_tbl" (
    "result_id" uuid not null,
    "tenant_id" uuid not null default ores_utility_system_tenant_id_fn(),
    "work_tenant_id" uuid not null,
    "workunit_id" uuid not null,
    "platforms" text[] not null,
    "priority" integer not null default 0,
    "state" text not null default 'queued',
    "queued_at" timestamp with time zone not null default now(),
    "host_id" uuid null,
    "claimed_at" timestamp with time zone null,
    "lease_expires_at" timestamp with time zone null,
    "attempts" integer not null default 0,
    primary key (result_id),
    check ("state" in ('queued', 'claimed'))
);

-- The next waiting entry: the claim reads the queued entries in priority order.
create index if not exists work_ledger_queued_idx
on "ores_compute_work_ledger_tbl" (priority desc, queued_at)
where state = 'queued';

-- The entries of one tenant, for the fair order and for the bound.
create index if not exists work_ledger_work_tenant_idx
on "ores_compute_work_ledger_tbl" (work_tenant_id, state);

-- The leases that have ended, for the reaper.
create index if not exists work_ledger_lease_idx
on "ores_compute_work_ledger_tbl" (lease_expires_at)
where state = 'claimed';

-- The entries a host holds, for the heartbeat.
create index if not exists work_ledger_host_idx
on "ores_compute_work_ledger_tbl" (host_id)
where state = 'claimed';

-- =============================================================================
-- Row-level security: the ledger belongs to the installation
-- Only a session of the system tenant reads or writes it, and only the system
-- tenant's own rows. No other tenant reads any entry, and an operator who has
-- entered a tenant reads none of it. The policy is written as an own-tenant
-- comparison that is also the system tenant, so it admits no tenant's rows to
-- anyone else and is not a widening.
-- =============================================================================
alter table ores_compute_work_ledger_tbl enable row level security;

drop policy if exists work_ledger_installation_policy
    on ores_compute_work_ledger_tbl;

create policy work_ledger_installation_policy
on ores_compute_work_ledger_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    and tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    and tenant_id = ores_utility_system_tenant_id_fn()
);
