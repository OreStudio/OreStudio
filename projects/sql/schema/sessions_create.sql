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
-- sessions table tracks individual user sessions with full lifecycle data.
--
-- If TimescaleDB is available:
--   - Table becomes a hypertable partitioned by start_time
--   - Automatic compression for chunks older than 7 days
--   - Automatic retention policy (1 year)
--
-- If TimescaleDB is NOT available:
--   - Regular PostgreSQL table with standard indexes
--   - Manual cleanup required for old data
--

--
-- Create the sessions table.
-- The composite primary key (id, start_time) works for both regular tables
-- and TimescaleDB hypertables.
--
create table if not exists "ores"."sessions" (
    -- Session identifier
    "id" uuid not null,

    -- Foreign key to accounts table
    "account_id" uuid not null,

    -- Session start timestamp (login time)
    -- Part of primary key for TimescaleDB hypertable partitioning
    "start_time" timestamp with time zone not null,

    -- Session end timestamp (logout or disconnect)
    -- Empty string if session is still active (sqlgen doesn't support NULL)
    "end_time" text not null default '',

    -- Client IP address (supports IPv4 and IPv6)
    -- Using text instead of inet for sqlgen compatibility
    "client_ip" text not null,

    -- Client application identifier from handshake
    "client_identifier" text not null default '',

    -- Client protocol version
    "client_version_major" smallint not null default 0,
    "client_version_minor" smallint not null default 0,

    -- Traffic metrics
    "bytes_sent" bigint not null default 0,
    "bytes_received" bigint not null default 0,

    -- Geolocation data (optional, based on IP lookup)
    -- Only country code is available from ip2country data source
    "country_code" text not null default '',

    -- Composite primary key: id + start_time
    -- Required for TimescaleDB, also works for regular tables
    primary key (id, start_time)
);

-- Indexes for common query patterns (work with both table types)
create index if not exists sessions_account_id_idx
on "ores"."sessions" (account_id, start_time desc);

create index if not exists sessions_active_idx
on "ores"."sessions" (account_id)
where end_time = '';

create index if not exists sessions_country_idx
on "ores"."sessions" (country_code, start_time desc)
where country_code != '';

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

        -- Convert to hypertable with 7-day chunks
        perform create_hypertable(
            'ores.sessions',
            'start_time',
            chunk_time_interval => interval '7 days',
            if_not_exists => true
        );
        raise notice 'Created hypertable with 7-day chunks';

        -- Enable compression for chunks older than 7 days
        alter table "ores"."sessions" set (
            timescaledb.compress,
            timescaledb.compress_segmentby = 'account_id',
            timescaledb.compress_orderby = 'start_time desc'
        );

        perform add_compression_policy(
            'ores.sessions',
            compress_after => interval '7 days',
            if_not_exists => true
        );
        raise notice 'Enabled compression policy (7 days)';

        -- Data retention policy: keep raw session data for 1 year
        perform add_retention_policy(
            'ores.sessions',
            drop_after => interval '1 year',
            if_not_exists => true
        );
        raise notice 'Enabled retention policy (1 year)';

        raise notice 'TimescaleDB setup complete for sessions table';
    else
        raise notice '================================================';
        raise notice 'TimescaleDB NOT available - using regular table';
        raise notice '================================================';
        raise notice 'Note: Manual cleanup of old session data will be required';
    end if;
end $$;
