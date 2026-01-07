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
set schema 'ores';

--
-- telemetry_logs table stores log entries from clients and server.
--
-- If TimescaleDB is available:
--   - Table becomes a hypertable partitioned by timestamp
--   - Automatic compression for chunks older than 3 days
--   - Automatic retention policy (30 days)
--
-- If TimescaleDB is NOT available:
--   - Regular PostgreSQL table with standard indexes
--   - Manual cleanup required for old data
--

--
-- Create the telemetry_logs table.
-- The composite primary key (id, timestamp) works for both regular tables
-- and TimescaleDB hypertables.
--
create table if not exists "ores"."telemetry_logs" (
    -- Log entry identifier
    "id" uuid not null,

    -- Log timestamp (when the log was emitted)
    -- Part of primary key for TimescaleDB hypertable partitioning
    "timestamp" timestamp with time zone not null,

    -- Source identification
    -- 'client' for logs from ores.qt, ores.shell, etc.
    -- 'server' for logs from ores.comms.service
    "source" text not null,

    -- Source application name
    -- e.g., 'ores.qt', 'ores.comms.shell', 'ores.comms.service'
    "source_name" text not null,

    -- Optional linkage to sessions table
    -- NULL for server logs or pre-login client logs
    "session_id" uuid,

    -- Optional linkage to accounts table
    -- NULL for server logs or pre-login client logs
    "account_id" uuid,

    -- Log severity level
    -- Valid values: trace, debug, info, warn, error
    "level" text not null,

    -- Logger/component name that emitted this log
    -- e.g., 'ores.qt.main_window', 'ores.comms.client'
    "component" text not null default '',

    -- The actual log message
    "message" text not null,

    -- Optional tag for filtering
    "tag" text not null default '',

    -- Server receipt timestamp
    "recorded_at" timestamp with time zone not null default now(),

    -- Composite primary key: id + timestamp
    -- Required for TimescaleDB, also works for regular tables
    primary key (id, timestamp)
);

-- Indexes for common query patterns (work with both table types)
create index if not exists telemetry_logs_session_idx
on "ores"."telemetry_logs" (session_id, timestamp desc)
where session_id is not null;

create index if not exists telemetry_logs_account_idx
on "ores"."telemetry_logs" (account_id, timestamp desc)
where account_id is not null;

create index if not exists telemetry_logs_level_idx
on "ores"."telemetry_logs" (level, timestamp desc);

create index if not exists telemetry_logs_source_idx
on "ores"."telemetry_logs" (source, source_name, timestamp desc);

create index if not exists telemetry_logs_component_idx
on "ores"."telemetry_logs" (component, timestamp desc)
where component != '';

--
-- TimescaleDB-specific setup (only if extension is installed)
--
do $$
declare
    tsdb_installed boolean;
begin
    -- Check if TimescaleDB extension is installed
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice '=========================================';
        raise notice 'TimescaleDB detected - creating hypertable';
        raise notice '=========================================';

        -- Convert to hypertable with 1-day chunks (logs are high volume)
        -- Use public schema prefix as search_path is set to 'ores'
        perform public.create_hypertable(
            'ores.telemetry_logs',
            'timestamp',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
        raise notice 'Created hypertable with 1-day chunks';

        -- Compression and retention policies require Timescale License (not Apache)
        -- Check if we have the community license before enabling these features
        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;

            if current_license = 'timescale' then
                -- Enable compression for chunks older than 3 days
                alter table "ores"."telemetry_logs" set (
                    timescaledb.compress,
                    timescaledb.compress_segmentby = 'source, source_name, level',
                    timescaledb.compress_orderby = 'timestamp desc'
                );

                perform public.add_compression_policy(
                    'ores.telemetry_logs',
                    compress_after => interval '3 days',
                    if_not_exists => true
                );
                raise notice 'Enabled compression policy (3 days)';

                -- Data retention policy: keep raw logs for 30 days
                perform public.add_retention_policy(
                    'ores.telemetry_logs',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
                raise notice 'Enabled retention policy (30 days)';
            else
                raise notice 'TimescaleDB Apache license - compression/retention policies skipped';
                raise notice 'Set timescaledb.license = ''timescale'' for full features';
            end if;
        end;

        raise notice 'TimescaleDB setup complete for telemetry_logs table';
    else
        raise notice '================================================';
        raise notice 'TimescaleDB NOT available - using regular table';
        raise notice '================================================';
        raise notice 'Note: Manual cleanup of old telemetry data will be required';
    end if;
end $$;
