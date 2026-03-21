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
-- Compute grid per-node telemetry samples.
-- Published by ores.compute.wrapper nodes every ~30 seconds via NATS and
-- persisted by ores.compute.service.
-- One row per node per sample interval; designed for TimescaleDB hypertable
-- partitioned by sampled_at.
-- =============================================================================

create table if not exists ores_compute_node_samples_tbl (
    "sampled_at"            timestamp with time zone not null,
    "tenant_id"             uuid                     not null,
    "host_id"               uuid                     not null,

    -- Cumulative task counters since wrapper process started
    "tasks_completed"       integer not null default 0,
    "tasks_failed"          integer not null default 0,

    -- Delta counters since the previous sample
    "tasks_since_last"      integer not null default 0,

    -- Average and peak task duration for tasks_since_last (milliseconds)
    "avg_task_duration_ms"  bigint  not null default 0,
    "max_task_duration_ms"  bigint  not null default 0,

    -- Bytes transferred for tasks_since_last
    "input_bytes_fetched"   bigint  not null default 0,
    "output_bytes_uploaded" bigint  not null default 0,

    -- Seconds elapsed since the last heartbeat was received for this host
    "seconds_since_hb"      integer not null default 0,

    primary key (sampled_at, tenant_id, host_id)
);

create index if not exists ores_compute_node_samples_host_idx
    on ores_compute_node_samples_tbl (host_id, sampled_at desc);

create index if not exists ores_compute_node_samples_tenant_idx
    on ores_compute_node_samples_tbl (tenant_id, sampled_at desc);

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        perform public.create_hypertable(
            'ores_compute_node_samples_tbl',
            'sampled_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );

        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true)
                into current_license;
            if current_license = 'timescale' then
                perform public.add_retention_policy(
                    'ores_compute_node_samples_tbl',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
            end if;
        end;
    end if;
end $$;
