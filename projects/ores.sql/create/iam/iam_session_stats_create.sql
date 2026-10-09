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
                return;
            end if;
        end;

        raise notice 'TimescaleDB detected - creating continuous aggregates';

        execute $sql$
            create materialized view if not exists ores_iam_session_stats_daily_vw
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
            from ores_iam_sessions_tbl
            where end_time != ''
            group by day, account_id
            with no data
        $sql$;

        perform public.add_continuous_aggregate_policy(
            'ores_iam_session_stats_daily_vw',
            start_offset => interval '3 days',
            end_offset => interval '1 hour',
            schedule_interval => interval '1 hour',
            if_not_exists => true
        );
        raise notice 'Created ores_iam_session_stats_daily_vw continuous aggregate';

        execute $sql$
            create materialized view if not exists ores_iam_session_stats_hourly_vw
            with (timescaledb.continuous) as
            select
                public.time_bucket('1 hour', start_time) as hour,
                account_id,
                count(*) as session_count,
                avg(extract(epoch from (end_time::timestamp with time zone - start_time))) as avg_duration_seconds,
                sum(bytes_sent) as total_bytes_sent,
                sum(bytes_received) as total_bytes_received
            from ores_iam_sessions_tbl
            where end_time != ''
            group by hour, account_id
            with no data
        $sql$;

        perform public.add_continuous_aggregate_policy(
            'ores_iam_session_stats_hourly_vw',
            start_offset => interval '1 day',
            end_offset => interval '15 minutes',
            schedule_interval => interval '15 minutes',
            if_not_exists => true
        );
        raise notice 'Created ores_iam_session_stats_hourly_vw continuous aggregate';

        execute $sql$
            create materialized view if not exists ores_iam_session_stats_aggregate_daily_vw
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
            from ores_iam_sessions_tbl
            where end_time != ''
            group by day
            with no data
        $sql$;

        perform public.add_continuous_aggregate_policy(
            'ores_iam_session_stats_aggregate_daily_vw',
            start_offset => interval '3 days',
            end_offset => interval '1 hour',
            schedule_interval => interval '1 hour',
            if_not_exists => true
        );
        raise notice 'Created ores_iam_session_stats_aggregate_daily_vw continuous aggregate';

        perform public.add_retention_policy(
            'ores_iam_session_stats_daily_vw',
            drop_after => interval '3 years',
            if_not_exists => true
        );

        perform public.add_retention_policy(
            'ores_iam_session_stats_hourly_vw',
            drop_after => interval '90 days',
            if_not_exists => true
        );
        raise notice 'Configured retention policies for continuous aggregates';

    else
        raise notice 'TimescaleDB NOT available - skipping continuous aggregates';
        raise notice 'Session statistics will require manual SQL queries';
    end if;
end $$;

create or replace function ores_iam_active_session_count_fn()
returns bigint
language sql
stable
as $$
    select count(*)
    from ores_iam_sessions_tbl
    where end_time = ''
    and tenant_id = ores_iam_current_tenant_id_fn();
$$;

create or replace function ores_iam_active_session_count_for_account_fn(p_account_id uuid)
returns bigint
language sql
stable
as $$
    select count(*)
    from ores_iam_sessions_tbl
    where account_id = p_account_id
    and end_time = ''
    and tenant_id = ores_iam_current_tenant_id_fn();
$$;

-- Tenant-aware active session count (for cross-tenant admin queries)
create or replace function ores_iam_active_session_count_for_tenant_fn(p_tenant_id uuid)
returns bigint
language sql
stable
as $$
    select count(*)
    from ores_iam_sessions_tbl
    where tenant_id = p_tenant_id and end_time = '';
$$;

-- -----------------------------------------------------------------------------
-- Session statistics for one tenant
-- -----------------------------------------------------------------------------
--
-- The continuous aggregates above are created only under the Timescale
-- licence. Under the Apache licence, which is what a development database
-- runs, they are absent, so a read that named them would fail rather than
-- answer. This read aggregates the sessions table itself, so the statistics
-- answer in either edition; the aggregates stay the cheaper path where they
-- exist, and a later task can switch this read to them.
--
-- The tenant scopes the rows, the account narrows them further, and the
-- window bounds the day the session started. The rows are one per day and
-- account, newest first.
create or replace function ores_iam_session_stats_for_tenant_fn(
    p_tenant_id uuid,
    p_account_id text default '',
    p_from timestamp with time zone default null,
    p_to timestamp with time zone default null,
    p_limit integer default 100,
    p_offset integer default 0
)
returns table (
    day timestamp with time zone,
    account_id text,
    session_count bigint,
    avg_duration_seconds double precision,
    total_bytes_sent bigint,
    total_bytes_received bigint,
    avg_bytes_sent double precision,
    avg_bytes_received double precision,
    unique_countries bigint
)
language sql
stable
as $$
    select
        date_trunc('day', s.start_time) as day,
        s.account_id::text,
        count(*) as session_count,
        avg(extract(epoch from (nullif(s.end_time, '')::timestamp with time zone
                                - s.start_time))) as avg_duration_seconds,
        sum(s.bytes_sent) as total_bytes_sent,
        sum(s.bytes_received) as total_bytes_received,
        avg(s.bytes_sent) as avg_bytes_sent,
        avg(s.bytes_received) as avg_bytes_received,
        count(distinct s.country_code) filter (where s.country_code <> '') as unique_countries
    from ores_iam_sessions_tbl s
    where s.tenant_id = p_tenant_id
      and (coalesce(p_account_id, '') = '' or s.account_id::text = p_account_id)
      and (p_from is null or s.start_time >= p_from)
      and (p_to is null or s.start_time <= p_to)
    group by 1, 2
    order by 1 desc, 2
    limit coalesce(nullif(p_limit, 0), 100)
    offset coalesce(p_offset, 0);
$$;
