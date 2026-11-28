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
 * The installation's own capacity, as a series.
 *
 * A store's size answers what one table holds. It does not answer the question a
 * disk asks: how large the database is, how much of the host is left, and how
 * long the remaining space lasts at the rate the database grows. Those are two
 * numbers taken at one instant, and only a series of them says where they are
 * going.
 *
 * The row states the database and the host it describes, because a size without
 * its host states nothing: the same database on a 40 GB partition and on a 2 TB
 * one are different problems. It states the path that was measured for the same
 * reason — the figure is the free space of one filesystem, and which filesystem
 * is not something a reader can infer.
 *
 * The free space can be absent. A host whose path the sampler could not read
 * records the database's size and says so in the note, because an unknown free
 * space and a full disk are different facts. A zero would read as a full disk.
 */

create table if not exists "ores_scheduler_capacity_samples_tbl" (
    "id" uuid not null default gen_random_uuid(),
    "sampled_at" timestamp with time zone not null,
    "tenant_id" uuid not null,
    "database_name" text not null,
    "host_name" text not null,
    "measured_path" text not null,
    "database_bytes" bigint not null default 0,
    "disk_free_bytes" bigint null,
    "disk_total_bytes" bigint null,
    "note" text not null default '',
    primary key (id, sampled_at),
    check ("database_name" <> ''),
    check ("host_name" <> ''),
    check ("measured_path" <> ''),
    check ("database_bytes" >= 0),
    check ("disk_free_bytes" is null or "disk_free_bytes" >= 0),
    check ("disk_total_bytes" is null or "disk_total_bytes" >= 0)
);

create index if not exists capacity_samples_sampled_at_idx
on "ores_scheduler_capacity_samples_tbl" ("sampled_at" desc);

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
            '"ores_scheduler_capacity_samples_tbl"',
            'sampled_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
        -- The community edition runs no retention job, so the samples are kept
        -- for ever until the housekeeping sweep can drop their chunks. The store
        -- is not in the catalogue yet, which the task that captures the
        -- catalogue's gap states.
        raise notice 'ores_scheduler_capacity_samples_tbl is chunked on the day; no retention policy is set on this licence';
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

-- =============================================================================
-- Row-level security: the installation's capacity is the system tenant's, read
-- by every session and written by the scheduler alone.
-- =============================================================================
alter table ores_scheduler_capacity_samples_tbl enable row level security;

drop policy if exists capacity_samples_tbl_tenant_isolation_policy
    on ores_scheduler_capacity_samples_tbl;

create policy capacity_samples_tbl_tenant_isolation_policy
on ores_scheduler_capacity_samples_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- The capacity of the installation is not a tenant's secret and not a tenant's
-- to change: every session reads the system tenant's rows, and only the system
-- tenant's session writes them.
drop policy if exists capacity_samples_tbl_shared_reference_read_policy
    on ores_scheduler_capacity_samples_tbl;

create policy capacity_samples_tbl_shared_reference_read_policy
on ores_scheduler_capacity_samples_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
);

comment on table ores_scheduler_capacity_samples_tbl is
'The installation''s size and the free space of the filesystem it sits on, one
 row per measurement. The database''s size is read from the catalogue; the free
 space is measured by the scheduler service on the host it runs on and can be
 absent, which the note explains.';
