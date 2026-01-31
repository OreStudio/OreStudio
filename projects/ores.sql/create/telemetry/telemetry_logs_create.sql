/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
-- Application log entries.
-- Designed for TimescaleDB hypertable.
-- Partitioned by timestamp.
-- =============================================================================

create table if not exists ores_telemetry_logs_tbl (
    "id" uuid not null,
    "timestamp" timestamp with time zone not null,
    "source" text not null,
    "source_name" text not null,
    "session_id" uuid,
    "account_id" uuid,
    "level" text not null,
    "component" text not null default '',
    "message" text not null,
    "tag" text not null default '',
    "recorded_at" timestamp with time zone not null default now(),
    primary key (id, timestamp)
);

create index if not exists ores_telemetry_logs_session_idx
on ores_telemetry_logs_tbl (session_id, timestamp desc)
where session_id is not null;

create index if not exists ores_telemetry_logs_account_idx
on ores_telemetry_logs_tbl (account_id, timestamp desc)
where account_id is not null;

create index if not exists ores_telemetry_logs_level_idx
on ores_telemetry_logs_tbl (level, timestamp desc);

create index if not exists ores_telemetry_logs_source_idx
on ores_telemetry_logs_tbl (source, source_name, timestamp desc);

create index if not exists ores_telemetry_logs_component_idx
on ores_telemetry_logs_tbl (component, timestamp desc)
where component != '';

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice '=========================================';
        raise notice 'TimescaleDB detected - creating hypertable';
        raise notice '=========================================';

        perform public.create_hypertable(
            'ores_telemetry_logs_tbl',
            'timestamp',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
        raise notice 'Created hypertable with 1-day chunks';

        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;

            if current_license = 'timescale' then
                alter table ores_telemetry_logs_tbl set (
                    timescaledb.compress,
                    timescaledb.compress_segmentby = 'source, source_name, level',
                    timescaledb.compress_orderby = 'timestamp desc'
                );

                perform public.add_compression_policy(
                    'ores_telemetry_logs_tbl',
                    compress_after => interval '3 days',
                    if_not_exists => true
                );
                raise notice 'Enabled compression policy (3 days)';

                perform public.add_retention_policy(
                    'ores_telemetry_logs_tbl',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
                raise notice 'Enabled retention policy (30 days)';
            else
                raise notice 'TimescaleDB Apache license - compression/retention policies skipped';
                raise notice 'Set timescaledb.license = ''timescale'' for full features';
            end if;
        end;

        raise notice 'TimescaleDB setup complete for ores_telemetry_logs_tbl table';
    else
        raise notice '================================================';
        raise notice 'TimescaleDB NOT available - using regular table';
        raise notice '================================================';
        raise notice 'Note: Manual cleanup of old telemetry data will be required';
    end if;
end $$;
