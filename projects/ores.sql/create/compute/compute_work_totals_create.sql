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
 * Work Totals Table
 *
 * The work of every tenant on the compute grid, summed, one row for each
 * sample. The poller of the grid samples writes it. It enters each tenant in
 * turn, reads that tenant's counters there, and adds them. No read crosses a
 * tenant, and no function runs with its owner's rights.
 *
 * A total of every tenant's work is a fact about several tenants at once, and
 * no tenant owns it, so it is not stored where a tenant reads. The table
 * belongs to the installation and holds counts only: no id of a tenant, a
 * party, a batch or a result. The policy admits only a session of the system
 * tenant, as the work ledger's does, so it is not a widening and a tenant
 * session reads none of it.
 *
 * The table is hand-written, not generated: it is a small installation-owned
 * series, a shape the entity templates do not offer.
 */

create table if not exists "ores_compute_work_totals_tbl" (
    "id" uuid not null,
    "sampled_at" timestamp with time zone not null,
    "tenant_id" uuid not null default ores_utility_system_tenant_id_fn(),
    "tenants_counted" integer not null default 0,
    "results_inactive" integer not null default 0,
    "results_unsent" integer not null default 0,
    "results_in_progress" integer not null default 0,
    "results_done" integer not null default 0,
    "total_workunits" integer not null default 0,
    "total_batches" integer not null default 0,
    "active_batches" integer not null default 0,
    "outcomes_success" integer not null default 0,
    "outcomes_client_error" integer not null default 0,
    "outcomes_no_reply" integer not null default 0,
    primary key (id, sampled_at)
);

create index if not exists work_totals_sampled_at_idx
on "ores_compute_work_totals_tbl" (sampled_at desc);

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice 'TimescaleDB detected - creating hypertable (1 day chunks)';

        perform public.create_hypertable(
            '"ores_compute_work_totals_tbl"',
            'sampled_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
        -- The same retention as the grid samples. The licence decides whether
        -- the policy can be set, and the community edition has no retention job.
        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;
            if current_license = 'timescale' then
                perform public.add_retention_policy(
                    '"ores_compute_work_totals_tbl"',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
            else
                raise notice 'TimescaleDB community edition - no retention policy is set for ores_compute_work_totals_tbl';
            end if;
        end;
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

-- Both comparisons are needed: the first is every table's own-tenant rule and the
-- second admits only the system tenant, which makes the series the installation's.
alter table ores_compute_work_totals_tbl enable row level security;

drop policy if exists work_totals_installation_policy
    on ores_compute_work_totals_tbl;

create policy work_totals_installation_policy
on ores_compute_work_totals_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    and tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    and tenant_id = ores_utility_system_tenant_id_fn()
);
