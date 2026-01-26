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
set schema 'production';

-- =============================================================================
-- User session tracking with country-level geolocation.
-- Designed for TimescaleDB hypertable.
-- Partitioned by start_time.
-- =============================================================================

create table if not exists "production"."iam_sessions_tbl" (
    "id" uuid not null,
    "account_id" uuid not null,
    "start_time" timestamp with time zone not null,
    "end_time" text not null default '',
    "client_ip" text not null,
    "client_identifier" text not null default '',
    "client_version_major" smallint not null default 0,
    "client_version_minor" smallint not null default 0,
    "bytes_sent" bigint not null default 0,
    "bytes_received" bigint not null default 0,
    "country_code" text not null default '',
    "protocol" text not null default 'binary',
    primary key (id, start_time)
);

create index if not exists iam_sessions_account_id_idx
on "production"."iam_sessions_tbl" (account_id, start_time desc);

create index if not exists iam_sessions_active_idx
on "production"."iam_sessions_tbl" (account_id)
where end_time = '';

create index if not exists iam_sessions_country_idx
on "production"."iam_sessions_tbl" (country_code, start_time desc)
where country_code != '';

create index if not exists iam_sessions_protocol_idx
on "production"."iam_sessions_tbl" (protocol, start_time desc);

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
            'production.iam_sessions_tbl',
            'start_time',
            chunk_time_interval => interval '7 days',
            if_not_exists => true
        );
        raise notice 'Created hypertable with 7-day chunks';

        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;

            if current_license = 'timescale' then
                alter table "production"."iam_sessions_tbl" set (
                    timescaledb.compress,
                    timescaledb.compress_segmentby = 'account_id',
                    timescaledb.compress_orderby = 'start_time desc'
                );

                perform public.add_compression_policy(
                    'production.iam_sessions_tbl',
                    compress_after => interval '7 days',
                    if_not_exists => true
                );
                raise notice 'Enabled compression policy (7 days)';

                perform public.add_retention_policy(
                    'production.iam_sessions_tbl',
                    drop_after => interval '1 year',
                    if_not_exists => true
                );
                raise notice 'Enabled retention policy (1 year)';
            else
                raise notice 'TimescaleDB Apache license - compression/retention policies skipped';
                raise notice 'Set timescaledb.license = ''timescale'' for full features';
            end if;
        end;

        raise notice 'TimescaleDB setup complete for iam_sessions_tbl table';
    else
        raise notice '================================================';
        raise notice 'TimescaleDB NOT available - using regular table';
        raise notice '================================================';
        raise notice 'Note: Manual cleanup of old session data will be required';
    end if;
end $$;
