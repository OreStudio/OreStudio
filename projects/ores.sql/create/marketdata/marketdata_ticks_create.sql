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

/**
 * Tick Table
 *
 * One append-only hypertable for the ticks of one retention class. A tick is a value of a
 * series at a time, and the series has one owner, a tenant and a party, so each row carries
 * the owner of its series. The ingest copies the owner from the series that it resolved.
 *
 * The table has no trigger, no valid_to and one index on the series and the time. A
 * correction is a new row, and the reader takes the latest. There is no notification trigger:
 * the live path publishes the tick, and the table is the history.
 *
 * Row-level security uses the policies of the other tables, in the scalar subquery form. The
 * session functions that the other tables call cost about 8 to 10 microseconds for each
 * filtered row, because the planner calls them again for every row, and a subquery around the
 * setting makes it evaluate the setting once for each query. FORCE makes the policies apply
 * to the owner too.
 *
 * The marketdata tick writer role has BYPASSRLS and INSERT only. PostgreSQL refuses COPY into
 * a table with row-level security for a role that the policies apply to, and this role is
 * exempt. It does not own the table and has no SELECT.
 *
 * The table is not compressed, because row-level security is not supported on compressed
 * chunks. Retention drops whole chunks. See doc/knowledge/architecture/tick_ingest_pipeline.org.
 *
 * Variables: :marketdata_writer_user
 */

create table if not exists "ores_marketdata_ticks_tbl" (
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "series_id" uuid not null,
    "ts" timestamp with time zone not null,
    "coord" smallint not null,
    "value" double precision not null,
    "source_id" uuid not null
);

create index if not exists marketdata_ticks_series_ts_idx
on "ores_marketdata_ticks_tbl" (series_id, ts desc);

do $$
declare
    tsdb_installed boolean;
    current_license text;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice 'TimescaleDB detected - creating hypertable (1 day chunks)';

        perform public.create_hypertable(
            '"ores_marketdata_ticks_tbl"',
            'ts',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );

        -- The licence decides whether the retention policy can be set. Without it, a
        -- job of the service drops the chunks that have aged out.
        select current_setting('timescaledb.license', true) into current_license;
        if current_license = 'timescale' then
            perform public.add_retention_policy(
                '"ores_marketdata_ticks_tbl"',
                drop_after => interval '7 days',
                if_not_exists => true
            );
        else
            raise notice 'TimescaleDB community edition - no retention policy is set for ores_marketdata_ticks_tbl';
        end if;
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

-- =============================================================================
-- Row-level security: tenant and party isolation for the ticks
-- =============================================================================
alter table ores_marketdata_ticks_tbl enable row level security;
alter table ores_marketdata_ticks_tbl force row level security;

drop policy if exists ticks_tbl_tenant_isolation_policy on ores_marketdata_ticks_tbl;

create policy ticks_tbl_tenant_isolation_policy
on ores_marketdata_ticks_tbl
for all using (
    tenant_id = (select nullif(current_setting('app.current_tenant_id', true), '')::uuid)
)
with check (
    tenant_id = (select nullif(current_setting('app.current_tenant_id', true), '')::uuid)
);

-- The form party_id = ANY((select ...)) does not compile, because PostgreSQL reads it as a
-- comparison with a subquery.
drop policy if exists ticks_tbl_party_isolation_policy on ores_marketdata_ticks_tbl;

create policy ticks_tbl_party_isolation_policy
on ores_marketdata_ticks_tbl
as restrictive
for select using (
    party_id in (select unnest(nullif(current_setting('app.visible_party_ids', true), '')::uuid[]))
);

grant insert on ores_marketdata_ticks_tbl to :marketdata_writer_user;
