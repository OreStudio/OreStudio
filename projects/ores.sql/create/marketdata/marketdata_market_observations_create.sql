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
 * Market Observation Table
 *
 * A single market data observation: the value of a series at a given
 * observation_datetime and optional point_id (tenor/surface coordinate).
 * observation_datetime is the financial valid-time (UTC); valid_from/valid_to
 * is the transaction time. Corrections replace the previous value via the
 * soft-update trigger.
 *
 * TimescaleDB hypertable partitioned by observation_datetime with 30-day chunks;
 * GIST exclusion and DELETE RULEs are incompatible with hypertables — uniqueness
 * is enforced via partial unique index and the soft-update/soft-delete trigger pair.
 *
 * No audit trail columns (version, modified_by, performed_by, change_reason_code,
 * change_commentary) — tick-level data volumes make these impractical.
 */

create table if not exists "ores_marketdata_market_observations_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "series_id" uuid not null,
    "observation_datetime" timestamp with time zone not null,
    "point_id" text null,
    "value" text not null,
    "source" text null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, observation_datetime),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("value" <> '')
);



create unique index if not exists market_observations_observations_current_uniq_idx
on "ores_marketdata_market_observations_tbl" (tenant_id, party_id, series_id, observation_datetime, coalesce(point_id, ''))
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists market_observations_observations_series_datetime_idx
on "ores_marketdata_market_observations_tbl" (tenant_id, party_id, series_id, observation_datetime desc);

create index if not exists market_observations_observations_tenant_datetime_idx
on "ores_marketdata_market_observations_tbl" (tenant_id, party_id, observation_datetime desc);

create index if not exists market_observations_observations_source_idx
on "ores_marketdata_market_observations_tbl" (tenant_id, party_id, source, observation_datetime desc)
where source is not null;

-- Supports SELECT DISTINCT ON (point_id) ... WHERE tenant_id = ? AND series_id = ? AND
-- observation_datetime <= ? ORDER BY point_id, observation_datetime DESC -- the as-of/
-- as-of-bucket curve snapshot queries. Ordered by point_id first (unlike
-- observations_series_datetime_idx, which orders by datetime globally) so Postgres can
-- skip-scan straight to each point's latest row instead of sorting the whole series.
create index if not exists market_observations_observations_series_point_datetime_idx
on "ores_marketdata_market_observations_tbl" (tenant_id, series_id, point_id, observation_datetime desc);

create or replace function ores_marketdata_market_observations_insert_fn()
returns trigger as $$
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    update "ores_marketdata_market_observations_tbl"
    set valid_to = current_timestamp
    where tenant_id = new.tenant_id
      and series_id = new.series_id
      and observation_datetime = new.observation_datetime
      and coalesce(point_id, '') = coalesce(new.point_id, '')
      and valid_to = ores_utility_infinity_timestamp_fn()
      and valid_from < current_timestamp;

    new.valid_from := current_timestamp;
    new.valid_to   := ores_utility_infinity_timestamp_fn();
    return new;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_market_observations_insert_trg
before insert on "ores_marketdata_market_observations_tbl"
for each row execute function ores_marketdata_market_observations_insert_fn();

create or replace function ores_marketdata_market_observations_delete_fn()
returns trigger as $$
begin
    update "ores_marketdata_market_observations_tbl"
    set valid_to = current_timestamp
    where tenant_id = old.tenant_id
      and series_id = old.series_id
      and observation_datetime = old.observation_datetime
      and coalesce(point_id, '') = coalesce(old.point_id, '')
      and valid_to = ores_utility_infinity_timestamp_fn();
    return null;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_market_observations_delete_trg
before delete on "ores_marketdata_market_observations_tbl"
for each row execute function ores_marketdata_market_observations_delete_fn();
do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice 'TimescaleDB detected - creating hypertable (30 days chunks)';

        perform public.create_hypertable(
            '"ores_marketdata_market_observations_tbl"',
            'observation_datetime',
            chunk_time_interval => interval '30 days',
            if_not_exists => true
        );
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;
