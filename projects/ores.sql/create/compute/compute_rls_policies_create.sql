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
-- Row-Level Security Policies for Compute Grid Tables
-- =============================================================================
-- Apps and app versions are system-owned global registries: readable by all
-- tenants, writable only by the current tenant (system admin writes them).
-- Hosts, batches, workunits, and results are strictly tenant-scoped.

-- -----------------------------------------------------------------------------
-- Apps (global registry — system tenant records visible to all tenants)
-- -----------------------------------------------------------------------------
alter table ores_compute_apps_tbl enable row level security;

create policy ores_compute_apps_tenant_isolation_policy
on ores_compute_apps_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()  -- system apps visible to all
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- App Versions (global registry — system tenant records visible to all tenants)
-- -----------------------------------------------------------------------------
alter table ores_compute_app_versions_tbl enable row level security;

create policy ores_compute_app_versions_tenant_isolation_policy
on ores_compute_app_versions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()  -- system app versions visible to all
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Hosts
-- -----------------------------------------------------------------------------
alter table ores_compute_hosts_tbl enable row level security;

create policy ores_compute_hosts_tenant_isolation_policy
on ores_compute_hosts_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Batches
-- -----------------------------------------------------------------------------
alter table ores_compute_batches_tbl enable row level security;

create policy ores_compute_batches_tenant_isolation_policy
on ores_compute_batches_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Batch Dependencies
-- -----------------------------------------------------------------------------
alter table ores_compute_batch_dependencies_tbl enable row level security;

create policy ores_compute_batch_dependencies_tenant_isolation_policy
on ores_compute_batch_dependencies_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Workunits
-- -----------------------------------------------------------------------------
alter table ores_compute_workunits_tbl enable row level security;

create policy ores_compute_workunits_tenant_isolation_policy
on ores_compute_workunits_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Results
-- -----------------------------------------------------------------------------
alter table ores_compute_results_tbl enable row level security;

create policy ores_compute_results_tenant_isolation_policy
on ores_compute_results_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
