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
-- NATS JetStream per-stream metrics samples.
-- Populated by ores.telemetry.service polling /jsz on the NATS monitoring API.
-- Designed for TimescaleDB hypertable, partitioned by sampled_at.
-- =============================================================================

create table if not exists ores_telemetry_nats_stream_samples_tbl (
    "sampled_at"      timestamp with time zone not null,
    "tenant_id"       uuid not null,
    "stream_name"     text not null,
    "messages"        bigint not null default 0,
    "bytes"           bigint not null default 0,
    "consumer_count"  integer not null default 0,
    primary key (sampled_at, tenant_id, stream_name)
);

create index if not exists ores_telemetry_nats_stream_samples_name_idx
on ores_telemetry_nats_stream_samples_tbl (stream_name, sampled_at desc);

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        perform public.create_hypertable(
            'ores_telemetry_nats_stream_samples_tbl',
            'sampled_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );

        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;
            if current_license = 'timescale' then
                perform public.add_retention_policy(
                    'ores_telemetry_nats_stream_samples_tbl',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
            end if;
        end;
    end if;
end $$;
