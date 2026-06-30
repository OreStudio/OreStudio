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
 * Market Fixing Table
 *
 * A historical index fixing: the official value of a fixing index (identified via
 * series_id) on a given fixing_date. Separate from observations: fixings are
 * historical facts (e.g. EUR-EURIBOR-3M fixed at X on date Y) rather than
 * forward-looking curve points.
 *
 * TimescaleDB hypertable partitioned by fixing_date with 30-day chunks.
 * Corrections use the soft-update/soft-delete pattern (no GIST, no DELETE RULEs).
 * No audit trail columns — volume makes them impractical.
 */

create table if not exists "ores_marketdata_market_fixings_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "series_id" uuid not null,
    "fixing_date" date not null,
    "value" text not null,
    "source" text null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, fixing_date),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("value" <> '')
);



create unique index if not exists market_fixings_fixings_current_uniq_idx
on "ores_marketdata_market_fixings_tbl" (tenant_id, series_id, fixing_date)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists market_fixings_fixings_series_date_idx
on "ores_marketdata_market_fixings_tbl" (tenant_id, series_id, fixing_date desc);

create index if not exists market_fixings_fixings_tenant_date_idx
on "ores_marketdata_market_fixings_tbl" (tenant_id, fixing_date desc);

create index if not exists market_fixings_fixings_source_idx
on "ores_marketdata_market_fixings_tbl" (tenant_id, source, fixing_date desc)
where source is not null;

create or replace function ores_marketdata_market_fixings_insert_fn()
returns trigger as $$
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    update "ores_marketdata_market_fixings_tbl"
    set valid_to = current_timestamp
    where tenant_id = new.tenant_id
      and series_id = new.series_id
      and fixing_date = new.fixing_date
      and valid_to = ores_utility_infinity_timestamp_fn()
      and valid_from < current_timestamp;

    new.valid_from := current_timestamp;
    new.valid_to   := ores_utility_infinity_timestamp_fn();
    return new;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_market_fixings_insert_trg
before insert on "ores_marketdata_market_fixings_tbl"
for each row execute function ores_marketdata_market_fixings_insert_fn();

create or replace function ores_marketdata_market_fixings_delete_fn()
returns trigger as $$
begin
    update "ores_marketdata_market_fixings_tbl"
    set valid_to = current_timestamp
    where tenant_id = old.tenant_id
      and series_id = old.series_id
      and fixing_date = old.fixing_date
      and valid_to = ores_utility_infinity_timestamp_fn();
    return null;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_market_fixings_delete_trg
before delete on "ores_marketdata_market_fixings_tbl"
for each row execute function ores_marketdata_market_fixings_delete_fn();
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
            '"ores_marketdata_market_fixings_tbl"',
            'fixing_date',
            chunk_time_interval => interval '30 days',
            if_not_exists => true
        );
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;
