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
-- Auth event telemetry: one row per authentication event (login, logout,
-- token refresh, max_session_exceeded). Designed for TimescaleDB hypertable.
-- Partitioned by event_time. No RLS — this is a system-level audit log
-- accessible only via the system DB role.
--
-- Event types:
--   login_success         - successful login
--   login_failure         - failed login attempt (wrong credentials, locked, etc.)
--   logout                - explicit logout
--   token_refresh         - JWT refreshed successfully
--   max_session_exceeded  - refresh rejected: max session duration reached
--   signup_success        - new account created via signup
--   signup_failure        - signup rejected
-- =============================================================================

create table if not exists ores_iam_auth_events_tbl (
    "id"            text not null,
    "event_time"    timestamp with time zone not null,
    "tenant_id"     text not null default '',
    "account_id"    text not null default '',
    "event_type"    text not null,
    "username"      text not null default '',
    "session_id"    text not null default '',
    "party_id"      text not null default '',
    "error_detail"  text not null default '',
    primary key (id, event_time)
);

create index if not exists ores_iam_auth_events_tenant_time_idx
on ores_iam_auth_events_tbl (tenant_id, event_time desc);

create index if not exists ores_iam_auth_events_event_type_idx
on ores_iam_auth_events_tbl (event_type, event_time desc);

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
            'ores_iam_auth_events_tbl',
            'event_time',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
        raise notice 'Created hypertable with 1-day chunks';

        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;

            if current_license = 'timescale' then
                -- Hourly aggregate: count of each event type per tenant
                execute $sql$
                    create materialized view if not exists ores_iam_auth_events_hourly_vw
                    with (timescaledb.continuous) as
                    select
                        public.time_bucket('1 hour', event_time) as hour,
                        tenant_id,
                        event_type,
                        count(*) as event_count
                    from ores_iam_auth_events_tbl
                    group by hour, tenant_id, event_type
                    with no data
                $sql$;

                perform public.add_continuous_aggregate_policy(
                    'ores_iam_auth_events_hourly_vw',
                    start_offset => interval '1 day',
                    end_offset => interval '15 minutes',
                    schedule_interval => interval '15 minutes',
                    if_not_exists => true
                );
                raise notice 'Created ores_iam_auth_events_hourly_vw continuous aggregate';

                -- Daily aggregate: count of each event type per tenant
                execute $sql$
                    create materialized view if not exists ores_iam_auth_events_daily_vw
                    with (timescaledb.continuous) as
                    select
                        public.time_bucket('1 day', event_time) as day,
                        tenant_id,
                        event_type,
                        count(*) as event_count,
                        count(distinct account_id) filter (
                            where account_id != '') as unique_accounts
                    from ores_iam_auth_events_tbl
                    group by day, tenant_id, event_type
                    with no data
                $sql$;

                perform public.add_continuous_aggregate_policy(
                    'ores_iam_auth_events_daily_vw',
                    start_offset => interval '3 days',
                    end_offset => interval '1 hour',
                    schedule_interval => interval '1 hour',
                    if_not_exists => true
                );
                raise notice 'Created ores_iam_auth_events_daily_vw continuous aggregate';

                -- Retention: keep raw events for 90 days, daily aggregates for 3 years
                perform public.add_retention_policy(
                    'ores_iam_auth_events_tbl',
                    drop_after => interval '90 days',
                    if_not_exists => true
                );
                perform public.add_retention_policy(
                    'ores_iam_auth_events_hourly_vw',
                    drop_after => interval '90 days',
                    if_not_exists => true
                );
                perform public.add_retention_policy(
                    'ores_iam_auth_events_daily_vw',
                    drop_after => interval '3 years',
                    if_not_exists => true
                );
                raise notice 'Configured retention policies for auth events';
            else
                raise notice 'TimescaleDB Apache license - continuous aggregates and retention policies skipped';
                raise notice 'Set timescaledb.license = ''timescale'' for full features';
            end if;
        end;

        raise notice 'TimescaleDB setup complete for ores_iam_auth_events_tbl table';
    else
        raise notice '================================================';
        raise notice 'TimescaleDB NOT available - using regular table';
        raise notice '================================================';
        raise notice 'Note: Manual cleanup of old auth event data will be required';
    end if;
end $$;
