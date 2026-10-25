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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * Tick Table
 *
 * A tick is a value of a coordinate of a stream at an instant. The stream is one
 * series of one provider, and its id is derived from the provider code and the
 * series URI, so the ingest writes the ticks of a source once and every party
 * reads the same rows. The ingest and the history read compute the id with
 * datum::stream_id.
 *
 * The table has no trigger, no valid_to and one index on the stream, the
 * coordinate and the time. A correction is a new row, and the reader takes the
 * latest. There is no notification trigger: the live path publishes the tick, and
 * the table is the history.
 *
 * The table belongs to no tenant and no party. Market data is an observation
 * about a source, so the row carries no owner and no row-level security policy
 * filters it. The history read checks the party's subscription to the provider of
 * the series before it reads a row, and that check is the only door to the ticks.
 * The tables are created by the schema owner and written by the ingest role.
 *
 * The marketdata tick writer role has INSERT only. It does not own the table and
 * has no SELECT.
 *
 * Retention drops whole chunks, so the store holds seven days of ticks. The table
 * is not compressed. See doc/knowledge/architecture/tick_ingest_pipeline.org.
 *
 * Variables: :marketdata_writer_user
 */

create table if not exists "ores_marketdata_ticks_tbl" (
    "stream_id" uuid not null,
    "source_id" uuid not null,
    "ts" timestamp with time zone not null,
    "coord" smallint not null,
    "value" double precision not null
);

create index if not exists marketdata_ticks_stream_coord_ts_idx
on "ores_marketdata_ticks_tbl" (stream_id, coord, ts desc);

do $$
declare
    tsdb_installed boolean;
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
        -- A hypertable grows without bound unless a retention policy drops the
        -- chunks that have aged out. The licence decides whether the policy can
        -- be set: the community edition has no retention job, so the model's
        -- interval is reported rather than silently ignored.
        declare
            current_license text;
        begin
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
        end;
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

grant insert on ores_marketdata_ticks_tbl to :marketdata_writer_user;
