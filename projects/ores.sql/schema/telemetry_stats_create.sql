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
-- Telemetry log statistics and helper functions.
--
-- If TimescaleDB is available (with Timescale license):
--   - Continuous aggregates for hourly/daily log metrics
--   - Auto-refresh policies for efficient incremental updates
--   - Retention policies for aggregated data (1 year)
--
-- If TimescaleDB is NOT available:
--   - Only helper functions are created
--   - Use standard SQL queries for telemetry statistics
--

--
-- TimescaleDB-specific: Continuous aggregates for telemetry statistics.
-- These are incrementally refreshed materialized views that provide
-- pre-computed metrics for fast dashboard queries.
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
        -- Continuous aggregates require Timescale License (not Apache)
        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;

            if current_license != 'timescale' then
                raise notice 'TimescaleDB Apache license - continuous aggregates not available';
                raise notice 'Set timescaledb.license = ''timescale'' for full features';
                return;
            end if;
        end;

        raise notice 'TimescaleDB detected - creating telemetry continuous aggregates';

        -- Hourly telemetry statistics by source, source_name, and level
        -- Useful for real-time monitoring and alerting
        execute $sql$
            create materialized view if not exists "ores"."telemetry_stats_hourly"
            with (timescaledb.continuous) as
            select
                public.time_bucket('1 hour', timestamp) as hour,
                source,
                source_name,
                level,
                count(*) as log_count,
                count(distinct session_id) filter (where session_id is not null) as unique_sessions,
                count(distinct account_id) filter (where account_id is not null) as unique_accounts
            from "ores"."telemetry_logs"
            group by hour, source, source_name, level
            with no data
        $sql$;

        -- Use public schema prefix as search_path is set to 'ores'
        perform public.add_continuous_aggregate_policy(
            'ores.telemetry_stats_hourly',
            start_offset => interval '1 day',
            end_offset => interval '15 minutes',
            schedule_interval => interval '15 minutes',
            if_not_exists => true
        );
        raise notice 'Created telemetry_stats_hourly continuous aggregate';

        -- Daily telemetry statistics with component breakdown
        -- Useful for trend analysis and capacity planning
        execute $sql$
            create materialized view if not exists "ores"."telemetry_stats_daily"
            with (timescaledb.continuous) as
            select
                public.time_bucket('1 day', timestamp) as day,
                source,
                source_name,
                component,
                level,
                count(*) as log_count,
                count(distinct session_id) filter (where session_id is not null) as unique_sessions,
                count(distinct account_id) filter (where account_id is not null) as unique_accounts
            from "ores"."telemetry_logs"
            group by day, source, source_name, component, level
            with no data
        $sql$;

        perform public.add_continuous_aggregate_policy(
            'ores.telemetry_stats_daily',
            start_offset => interval '3 days',
            end_offset => interval '1 hour',
            schedule_interval => interval '1 hour',
            if_not_exists => true
        );
        raise notice 'Created telemetry_stats_daily continuous aggregate';

        -- Aggregate daily statistics across all sources (system-wide overview)
        -- Useful for executive dashboards
        execute $sql$
            create materialized view if not exists "ores"."telemetry_stats_aggregate_daily"
            with (timescaledb.continuous) as
            select
                public.time_bucket('1 day', timestamp) as day,
                source,
                level,
                count(*) as log_count,
                count(distinct source_name) as unique_source_names,
                count(distinct component) filter (where component != '') as unique_components,
                count(distinct session_id) filter (where session_id is not null) as unique_sessions,
                count(distinct account_id) filter (where account_id is not null) as unique_accounts
            from "ores"."telemetry_logs"
            group by day, source, level
            with no data
        $sql$;

        perform public.add_continuous_aggregate_policy(
            'ores.telemetry_stats_aggregate_daily',
            start_offset => interval '3 days',
            end_offset => interval '1 hour',
            schedule_interval => interval '1 hour',
            if_not_exists => true
        );
        raise notice 'Created telemetry_stats_aggregate_daily continuous aggregate';

        -- Retention policies for continuous aggregates
        -- Keep aggregated stats for 1 year (longer than raw logs at 30 days)
        perform public.add_retention_policy(
            'ores.telemetry_stats_hourly',
            drop_after => interval '90 days',
            if_not_exists => true
        );

        perform public.add_retention_policy(
            'ores.telemetry_stats_daily',
            drop_after => interval '1 year',
            if_not_exists => true
        );

        perform public.add_retention_policy(
            'ores.telemetry_stats_aggregate_daily',
            drop_after => interval '1 year',
            if_not_exists => true
        );
        raise notice 'Configured retention policies for telemetry continuous aggregates';

    else
        raise notice 'TimescaleDB NOT available - skipping telemetry continuous aggregates';
        raise notice 'Telemetry statistics will require manual SQL queries';
    end if;
end $$;

--
-- Helper function to get log count for the last N hours.
-- Works with or without TimescaleDB.
--
create or replace function ores.telemetry_log_count_last_hours(p_hours integer default 24)
returns bigint
language sql
stable
as $$
    select count(*)
    from ores.telemetry_logs
    where timestamp > now() - make_interval(hours => p_hours);
$$;

--
-- Helper function to get log count by level for the last N hours.
-- Works with or without TimescaleDB.
--
create or replace function ores.telemetry_log_count_by_level(
    p_level text,
    p_hours integer default 24
)
returns bigint
language sql
stable
as $$
    select count(*)
    from ores.telemetry_logs
    where level = p_level
      and timestamp > now() - make_interval(hours => p_hours);
$$;

--
-- Helper function to get error count for a specific source.
-- Useful for monitoring dashboards and alerting.
--
create or replace function ores.telemetry_error_count(
    p_source_name text,
    p_hours integer default 1
)
returns bigint
language sql
stable
as $$
    select count(*)
    from ores.telemetry_logs
    where source_name = p_source_name
      and level = 'error'
      and timestamp > now() - make_interval(hours => p_hours);
$$;

--
-- Helper function to get logs for a specific session.
-- Useful for debugging client issues.
--
create or replace function ores.telemetry_logs_for_session(
    p_session_id uuid,
    p_limit integer default 1000
)
returns table (
    id uuid,
    log_timestamp timestamp with time zone,
    level text,
    component text,
    message text,
    tag text
)
language sql
stable
as $$
    select id, timestamp, level, component, message, tag
    from ores.telemetry_logs
    where session_id = p_session_id
    order by timestamp desc
    limit p_limit;
$$;
