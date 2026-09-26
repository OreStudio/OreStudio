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
 * Node Sample Table
 *
 * One wrapper node's report, published fire-and-forget on
 * compute.v1.telemetry.node_samples and written here by the service. The
 * rows are append-only, one per node per interval, and the dashboard shows
 * the newest row of each node.
 *
 * The model states :current_state: for the same reason grid_sample does:
 * a sample is a fact about an instant, so the bi-temporal pair has no
 * meaning on it.
 *
 * The per-node newest read stays hand-written. It is a
 * DISTINCT ON (host_id) over the tenant's rows, which no generated read
 * expresses: a read by host_id returns one node's rows, and a read of the
 * newest returns the grid's newest and not one row per node.
 */

create table if not exists "ores_compute_node_samples_tbl" (
    "id" uuid not null,
    "sampled_at" timestamp with time zone not null,
    "tenant_id" uuid not null,
    "host_id" uuid not null,
    "tasks_completed" integer not null default 0,
    "tasks_failed" integer not null default 0,
    "tasks_since_last" integer not null default 0,
    "avg_task_duration_ms" bigint not null default 0,
    "max_task_duration_ms" bigint not null default 0,
    "input_bytes_fetched" bigint not null default 0,
    "output_bytes_uploaded" bigint not null default 0,
    "seconds_since_hb" integer not null default 0,
    primary key (id, sampled_at)
);



create index if not exists node_samples_node_samples_host_idx
on "ores_compute_node_samples_tbl" (host_id, sampled_at desc);

create index if not exists node_samples_node_samples_tenant_idx
on "ores_compute_node_samples_tbl" (tenant_id, sampled_at desc);

create or replace function ores_compute_node_samples_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_compute_node_samples_insert_trg
before insert on "ores_compute_node_samples_tbl"
for each row execute function ores_compute_node_samples_insert_fn();

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
            '"ores_compute_node_samples_tbl"',
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
                    '"ores_compute_node_samples_tbl"',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
            else
                raise notice 'TimescaleDB community edition - no retention policy is set for ores_compute_node_samples_tbl';
            end if;
        end;
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

-- =============================================================================
-- Row-level security: tenant isolation for Node Sample
-- =============================================================================
alter table ores_compute_node_samples_tbl enable row level security;

create policy node_samples_tbl_tenant_isolation_policy
on ores_compute_node_samples_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
