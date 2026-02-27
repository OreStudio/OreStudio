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
-- Session time-series samples: bytes captured at heartbeat frequency.
-- Designed for TimescaleDB hypertable.
-- Partitioned by sample_time.
-- =============================================================================

create table if not exists ores_iam_session_samples_tbl (
    "session_id" uuid not null,
    "tenant_id" uuid not null,
    "sample_time" timestamp with time zone not null,
    "bytes_sent" bigint not null default 0,
    "bytes_received" bigint not null default 0,
    "latency_ms" bigint not null default 0,
    primary key (session_id, sample_time)
);

create index if not exists ores_iam_session_samples_session_idx
on ores_iam_session_samples_tbl (session_id, sample_time desc);

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
            'ores_iam_session_samples_tbl',
            'sample_time',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
        raise notice 'Created hypertable with 1-day chunks';

        -- NOTE: Compression (columnstore) is intentionally NOT enabled on this
        -- table. TimescaleDB 2.16+ columnstore is incompatible with Row Level
        -- Security, which ores_iam_session_samples_tbl requires for tenant isolation.
        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;

            if current_license = 'timescale' then
                perform public.add_retention_policy(
                    'ores_iam_session_samples_tbl',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
                raise notice 'Enabled retention policy (30 days)';
            else
                raise notice 'TimescaleDB Apache license - retention policy skipped';
                raise notice 'Set timescaledb.license = ''timescale'' for full features';
            end if;
        end;

        raise notice 'TimescaleDB setup complete for ores_iam_session_samples_tbl table';
    else
        raise notice '================================================';
        raise notice 'TimescaleDB NOT available - using regular table';
        raise notice '================================================';
        raise notice 'Note: Manual cleanup of old sample data will be required';
    end if;
end $$;
