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
set schema 'oresdb';

--
-- Enable TimescaleDB extension if not already enabled.
-- TimescaleDB provides automatic time-based partitioning, compression,
-- continuous aggregates, and data retention policies.
--
create extension if not exists timescaledb;

--
-- sessions table tracks individual user sessions with full lifecycle data.
-- This is a TimescaleDB hypertable partitioned by start_time for efficient
-- time-series queries.
--
create table if not exists "oresdb"."sessions" (
    -- Session identifier
    "id" uuid not null,

    -- Foreign key to accounts table
    "account_id" uuid not null,

    -- Cached admin status for the session
    "is_admin" integer not null default 0,

    -- Session start timestamp (login time)
    -- Part of primary key for TimescaleDB hypertable partitioning
    "start_time" timestamp with time zone not null,

    -- Session end timestamp (logout or disconnect)
    -- NULL if session is still active
    "end_time" timestamp with time zone,

    -- Client IP address (supports IPv4 and IPv6)
    "client_ip" inet not null,

    -- Client application identifier from handshake
    "client_identifier" text not null default '',

    -- Client protocol version
    "client_version_major" smallint not null default 0,
    "client_version_minor" smallint not null default 0,

    -- Traffic metrics
    "bytes_sent" bigint not null default 0,
    "bytes_received" bigint not null default 0,

    -- Geolocation data (optional, based on IP lookup)
    "country_code" text not null default '',
    "city" text not null default '',
    "latitude" double precision,
    "longitude" double precision,

    -- Composite primary key: id + start_time required for TimescaleDB
    primary key (id, start_time)
);

-- Convert to hypertable with 7-day chunks
-- This enables automatic time-based partitioning
select create_hypertable(
    'oresdb.sessions',
    'start_time',
    chunk_time_interval => interval '7 days',
    if_not_exists => true
);

-- Indexes for common query patterns
create index if not exists sessions_account_id_idx
on "oresdb"."sessions" (account_id, start_time desc);

create index if not exists sessions_active_idx
on "oresdb"."sessions" (account_id)
where end_time is null;

create index if not exists sessions_country_idx
on "oresdb"."sessions" (country_code, start_time desc)
where country_code != '';

-- Enable compression for chunks older than 7 days
-- Segment by account_id for efficient per-account queries
alter table "oresdb"."sessions" set (
    timescaledb.compress,
    timescaledb.compress_segmentby = 'account_id',
    timescaledb.compress_orderby = 'start_time desc'
);

select add_compression_policy(
    'oresdb.sessions',
    compress_after => interval '7 days',
    if_not_exists => true
);

-- Data retention policy: keep raw session data for 1 year
select add_retention_policy(
    'oresdb.sessions',
    drop_after => interval '1 year',
    if_not_exists => true
);
