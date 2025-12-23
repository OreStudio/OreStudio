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
-- Continuous aggregates for session statistics.
-- These are incrementally refreshed materialized views that provide
-- pre-computed metrics for fast dashboard queries.
--

--
-- Daily session statistics per account.
-- Provides average duration, bytes transferred, and session counts.
--
create materialized view if not exists "oresdb"."session_stats_daily"
with (timescaledb.continuous) as
select
    time_bucket('1 day', start_time) as day,
    account_id,
    count(*) as session_count,
    avg(extract(epoch from (end_time - start_time))) as avg_duration_seconds,
    sum(bytes_sent) as total_bytes_sent,
    sum(bytes_received) as total_bytes_received,
    avg(bytes_sent) as avg_bytes_sent,
    avg(bytes_received) as avg_bytes_received,
    count(distinct country_code) filter (where country_code != '') as unique_countries,
    count(distinct city) filter (where city != '') as unique_cities
from "oresdb"."sessions"
where end_time is not null
group by day, account_id
with no data;

-- Auto-refresh policy: refresh data up to 1 hour ago, every hour
select add_continuous_aggregate_policy(
    'oresdb.session_stats_daily',
    start_offset => interval '3 days',
    end_offset => interval '1 hour',
    schedule_interval => interval '1 hour',
    if_not_exists => true
);

--
-- Hourly session statistics per account.
-- More granular view for recent activity analysis.
--
create materialized view if not exists "oresdb"."session_stats_hourly"
with (timescaledb.continuous) as
select
    time_bucket('1 hour', start_time) as hour,
    account_id,
    count(*) as session_count,
    avg(extract(epoch from (end_time - start_time))) as avg_duration_seconds,
    sum(bytes_sent) as total_bytes_sent,
    sum(bytes_received) as total_bytes_received
from "oresdb"."sessions"
where end_time is not null
group by hour, account_id
with no data;

-- Refresh hourly stats every 15 minutes
select add_continuous_aggregate_policy(
    'oresdb.session_stats_hourly',
    start_offset => interval '1 day',
    end_offset => interval '15 minutes',
    schedule_interval => interval '15 minutes',
    if_not_exists => true
);

--
-- Aggregate daily statistics across all accounts.
-- Provides system-wide metrics for admin dashboards.
--
create materialized view if not exists "oresdb"."session_stats_aggregate_daily"
with (timescaledb.continuous) as
select
    time_bucket('1 day', start_time) as day,
    count(*) as session_count,
    count(distinct account_id) as unique_accounts,
    avg(extract(epoch from (end_time - start_time))) as avg_duration_seconds,
    sum(bytes_sent) as total_bytes_sent,
    sum(bytes_received) as total_bytes_received,
    avg(bytes_sent) as avg_bytes_sent,
    avg(bytes_received) as avg_bytes_received,
    count(distinct country_code) filter (where country_code != '') as unique_countries,
    count(distinct city) filter (where city != '') as unique_cities,
    -- Peak concurrent sessions approximation (sessions starting in this bucket)
    count(*) as sessions_started
from "oresdb"."sessions"
where end_time is not null
group by day
with no data;

-- Refresh aggregate daily stats every hour
select add_continuous_aggregate_policy(
    'oresdb.session_stats_aggregate_daily',
    start_offset => interval '3 days',
    end_offset => interval '1 hour',
    schedule_interval => interval '1 hour',
    if_not_exists => true
);

--
-- Retention policy for continuous aggregates.
-- Keep aggregated stats longer than raw data.
--
-- Daily stats: keep for 3 years
select add_retention_policy(
    'oresdb.session_stats_daily',
    drop_after => interval '3 years',
    if_not_exists => true
);

-- Hourly stats: keep for 90 days (more granular = shorter retention)
select add_retention_policy(
    'oresdb.session_stats_hourly',
    drop_after => interval '90 days',
    if_not_exists => true
);

-- Aggregate daily stats: keep indefinitely (no retention policy)
-- These are small and provide historical system-wide trends

--
-- Helper function to get current active session count.
--
create or replace function oresdb.active_session_count()
returns bigint
language sql
stable
as $$
    select count(*) from oresdb.sessions where end_time is null;
$$;

--
-- Helper function to get active session count for an account.
--
create or replace function oresdb.active_session_count_for_account(p_account_id uuid)
returns bigint
language sql
stable
as $$
    select count(*)
    from oresdb.sessions
    where account_id = p_account_id and end_time is null;
$$;
