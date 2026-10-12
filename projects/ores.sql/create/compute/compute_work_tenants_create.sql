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
 * Work Tenants Table
 *
 * The tenants that have dispatched work to the compute grid. The poller of the
 * grid samples writes one row of counters for each of these tenants, and it
 * cannot list them from the work itself: a tenant's batches are the tenant's
 * and no policy lets the system tenant read them, and the compute service has
 * no read of the tenant roster.
 *
 * The table belongs to the installation. It holds a tenant id and a time, and
 * no business data. The dispatcher adds a tenant the first time it dispatches
 * for it, as the system tenant, and the poller reads the list as the system
 * tenant. The policy admits only a session of the system tenant, as the work
 * ledger's does, so it is not a widening.
 *
 * The table is hand-written, not generated: it is a small installation-owned
 * list, a shape the entity templates do not offer.
 */

create table if not exists "ores_compute_work_tenants_tbl" (
    "work_tenant_id" uuid not null,
    "tenant_id" uuid not null default ores_utility_system_tenant_id_fn(),
    "first_seen_at" timestamp with time zone not null default now(),
    primary key (work_tenant_id)
);

-- Both comparisons are needed: the first is every table's own-tenant rule and the
-- second admits only the system tenant, which makes the list the installation's.
alter table ores_compute_work_tenants_tbl enable row level security;

drop policy if exists work_tenants_installation_policy
    on ores_compute_work_tenants_tbl;

create policy work_tenants_installation_policy
on ores_compute_work_tenants_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    and tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    and tenant_id = ores_utility_system_tenant_id_fn()
);
