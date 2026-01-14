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

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
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

        raise notice 'TimescaleDB detected - creating continuous aggregates';

        execute $sql$
            create materialized view if not exists "ores"."iam_session_stats_daily"
            with (timescaledb.continuous) as
            select
                public.time_bucket('1 day', start_time) as day,
                account_id,
                count(*) as session_count,
                avg(extract(epoch from (end_time::timestamp with time zone - start_time))) as avg_duration_seconds,
                sum(bytes_sent) as total_bytes_sent,
                sum(bytes_received) as total_bytes_received,
                avg(bytes_sent) as avg_bytes_sent,
                avg(bytes_received) as avg_bytes_received,
                count(distinct country_code) filter (where country_code != '') as unique_countries
            from "ores"."iam_sessions_tbl"
            where end_time != ''
            group by day, account_id
            with no data
        $sql$;

        perform public.add_continuous_aggregate_policy(
            'ores.iam_session_stats_daily',
            start_offset => interval '3 days',
            end_offset => interval '1 hour',
            schedule_interval => interval '1 hour',
            if_not_exists => true
        );
        raise notice 'Created iam_session_stats_daily continuous aggregate';

        execute $sql$
            create materialized view if not exists "ores"."iam_session_stats_hourly"
            with (timescaledb.continuous) as
            select
                public.time_bucket('1 hour', start_time) as hour,
                account_id,
                count(*) as session_count,
                avg(extract(epoch from (end_time::timestamp with time zone - start_time))) as avg_duration_seconds,
                sum(bytes_sent) as total_bytes_sent,
                sum(bytes_received) as total_bytes_received
            from "ores"."iam_sessions_tbl"
            where end_time != ''
            group by hour, account_id
            with no data
        $sql$;

        perform public.add_continuous_aggregate_policy(
            'ores.iam_session_stats_hourly',
            start_offset => interval '1 day',
            end_offset => interval '15 minutes',
            schedule_interval => interval '15 minutes',
            if_not_exists => true
        );
        raise notice 'Created iam_session_stats_hourly continuous aggregate';

        execute $sql$
            create materialized view if not exists "ores"."iam_session_stats_aggregate_daily"
            with (timescaledb.continuous) as
            select
                public.time_bucket('1 day', start_time) as day,
                count(*) as session_count,
                count(distinct account_id) as unique_accounts,
                avg(extract(epoch from (end_time::timestamp with time zone - start_time))) as avg_duration_seconds,
                sum(bytes_sent) as total_bytes_sent,
                sum(bytes_received) as total_bytes_received,
                avg(bytes_sent) as avg_bytes_sent,
                avg(bytes_received) as avg_bytes_received,
                count(distinct country_code) filter (where country_code != '') as unique_countries,
                count(*) as sessions_started
            from "ores"."iam_sessions_tbl"
            where end_time != ''
            group by day
            with no data
        $sql$;

        perform public.add_continuous_aggregate_policy(
            'ores.iam_session_stats_aggregate_daily',
            start_offset => interval '3 days',
            end_offset => interval '1 hour',
            schedule_interval => interval '1 hour',
            if_not_exists => true
        );
        raise notice 'Created iam_session_stats_aggregate_daily continuous aggregate';

        perform public.add_retention_policy(
            'ores.iam_session_stats_daily',
            drop_after => interval '3 years',
            if_not_exists => true
        );

        perform public.add_retention_policy(
            'ores.iam_session_stats_hourly',
            drop_after => interval '90 days',
            if_not_exists => true
        );
        raise notice 'Configured retention policies for continuous aggregates';

    else
        raise notice 'TimescaleDB NOT available - skipping continuous aggregates';
        raise notice 'Session statistics will require manual SQL queries';
    end if;
end $$;

create or replace function ores.iam_active_session_count_fn()
returns bigint
language sql
stable
as $$
    select count(*) from ores.iam_sessions_tbl where end_time = '';
$$;

create or replace function ores.iam_active_session_count_for_account_fn(p_account_id uuid)
returns bigint
language sql
stable
as $$
    select count(*)
    from ores.iam_sessions_tbl
    where account_id = p_account_id and end_time = '';
$$;
