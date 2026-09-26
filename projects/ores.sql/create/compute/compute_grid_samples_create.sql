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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * Grid Sample Table
 *
 * One snapshot of the grid, written by the compute service's poller on its
 * own interval. The counters mirror what the database holds at that moment,
 * so a dashboard reads one stored row rather than aggregating on every
 * refresh.
 *
 * The table is append-only and carries no validity window: a sample is a
 * fact about an instant, and nothing edits it. That is why the model states
 * :current_state:, which drops the transaction-time pair the bi-temporal
 * shape would add and lets the partition column sit in the primary key.
 *
 * The reads the codegen cannot express live beside it. A per-node newest row
 * is a DISTINCT ON (host_id) and the live summary is a call to
 * ores_compute_grid_stats_fn, so both stay hand-written in
 * compute_telemetry_repository.
 */

create table if not exists "ores_compute_grid_samples_tbl" (
    "id" uuid not null,
    "sampled_at" timestamp with time zone not null,
    "tenant_id" uuid not null,
    "total_hosts" integer not null default 0,
    "online_hosts" integer not null default 0,
    "idle_hosts" integer not null default 0,
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



create index if not exists grid_samples_grid_samples_tenant_idx
on "ores_compute_grid_samples_tbl" (tenant_id, sampled_at desc);

create or replace function ores_compute_grid_samples_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_compute_grid_samples_insert_trg
before insert on "ores_compute_grid_samples_tbl"
for each row execute function ores_compute_grid_samples_insert_fn();

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
            '"ores_compute_grid_samples_tbl"',
            'sampled_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
        -- A hypertable grows without bound unless a retention policy drops the
        -- chunks that have aged out. The licence decides whether the policy can
        -- be set: the community edition has no retention job, so the model's
        -- interval is reported rather than silently ignored.
        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;
            if current_license = 'timescale' then
                perform public.add_retention_policy(
                    '"ores_compute_grid_samples_tbl"',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
            else
                raise notice 'TimescaleDB community edition - no retention policy is set for ores_compute_grid_samples_tbl';
            end if;
        end;
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

-- =============================================================================
-- Row-level security: tenant isolation for Grid Sample
-- =============================================================================
alter table ores_compute_grid_samples_tbl enable row level security;

create policy grid_samples_tbl_tenant_isolation_policy
on ores_compute_grid_samples_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
