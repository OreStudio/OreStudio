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
-- Market data observations.
-- One row per (series, observation_date, point_id). point_id is the tenor or
-- compound surface identifier (e.g. "1Y", "5Y/2Y/ATM", "0.03/10Y/2Y"); it is
-- null for scalar series such as spot FX rates.
--
-- Bitemporal: observation_date is the financial valid-time (when the market
-- was observed); valid_from/valid_to is the transaction time (when this record
-- was current in the system). See series table for the bitemporality rationale.
--
-- TimescaleDB hypertable partitioned by observation_date with 30-day chunks.
-- GIST exclusion is incompatible with hypertables; uniqueness of current rows
-- is enforced via partial unique index + insert trigger instead.
-- =============================================================================

create table if not exists ores_marketdata_observations_tbl (
    "id"               uuid not null,
    "tenant_id"        uuid not null,
    "series_id"        uuid not null,
    "observation_date" date not null,
    "point_id"         text,
    "value"            text not null,
    "source"           text,
    "valid_from"       timestamp with time zone not null,
    "valid_to"         timestamp with time zone not null,
    primary key (id, observation_date),
    check ("valid_from" < "valid_to"),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("value" <> '')
);

-- Current-row uniqueness per (tenant, series, date, point).
-- coalesce(point_id, '') maps scalars to an empty string so the index covers them.
create unique index if not exists ores_marketdata_observations_current_uniq_idx
on ores_marketdata_observations_tbl (tenant_id, series_id, observation_date, coalesce(point_id, ''))
where valid_to = ores_utility_infinity_timestamp_fn();

-- Lookup by series + date range (primary query pattern).
create index if not exists ores_marketdata_observations_series_date_idx
on ores_marketdata_observations_tbl (tenant_id, series_id, observation_date desc);

-- Lookup by tenant across all series for a date.
create index if not exists ores_marketdata_observations_tenant_date_idx
on ores_marketdata_observations_tbl (tenant_id, observation_date desc);

-- Lookup by source.
create index if not exists ores_marketdata_observations_source_idx
on ores_marketdata_observations_tbl (tenant_id, source, observation_date desc)
where source is not null;

-- =============================================================================
-- Insert trigger — implements the soft-update pattern for corrections:
--   Inserting a row for an existing (series, date, point) closes the old row
--   and replaces it, preserving the full history of values at that point.
-- =============================================================================
create or replace function ores_marketdata_observations_insert_fn()
returns trigger as $$
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    update ores_marketdata_observations_tbl
    set valid_to = current_timestamp
    where tenant_id        = new.tenant_id
      and series_id        = new.series_id
      and observation_date = new.observation_date
      and coalesce(point_id, '') = coalesce(new.point_id, '')
      and valid_to         = ores_utility_infinity_timestamp_fn()
      and valid_from       < current_timestamp;

    new.valid_from := current_timestamp;
    new.valid_to   := ores_utility_infinity_timestamp_fn();
    return new;
end;
$$ language plpgsql;

create or replace trigger ores_marketdata_observations_insert_trg
before insert on ores_marketdata_observations_tbl
for each row execute function ores_marketdata_observations_insert_fn();

-- =============================================================================
-- Soft-delete rule.
-- =============================================================================
create or replace rule ores_marketdata_observations_delete_rule as
on delete to ores_marketdata_observations_tbl do instead
    update ores_marketdata_observations_tbl
    set valid_to = current_timestamp
    where tenant_id        = old.tenant_id
      and series_id        = old.series_id
      and observation_date = old.observation_date
      and coalesce(point_id, '') = coalesce(old.point_id, '')
      and valid_to         = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- TimescaleDB hypertable.
-- =============================================================================
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
            'ores_marketdata_observations_tbl',
            'observation_date',
            chunk_time_interval => interval '30 days',
            if_not_exists => true
        );
        raise notice 'Created hypertable with 30-day chunks';
        raise notice 'TimescaleDB setup complete for ores_marketdata_observations_tbl';
    else
        raise notice '================================================';
        raise notice 'TimescaleDB NOT available - using regular table';
        raise notice '================================================';
    end if;
end $$;
