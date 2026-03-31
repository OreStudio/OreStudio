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
-- Market data fixings (historical index realisations).
-- Separate from observations: fixings are historical facts (e.g. EUR-EURIBOR-3M
-- fixed at X on date Y) rather than forward-looking curve points. The fixing
-- index is identified via series_id → ores_marketdata_series_tbl.
--
-- TimescaleDB hypertable partitioned by fixing_date with 30-day chunks.
-- Bitemporality enforced via partial unique index + insert trigger (same
-- pattern as observations — no GIST on hypertables).
-- =============================================================================

create table if not exists ores_marketdata_fixings_tbl (
    "id"           uuid not null,
    "tenant_id"    uuid not null,
    "series_id"    uuid not null,
    "fixing_date"  date not null,
    "value"        text not null,
    "source"       text,
    "valid_from"   timestamp with time zone not null,
    "valid_to"     timestamp with time zone not null,
    primary key (id, fixing_date),
    check ("valid_from" < "valid_to"),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("value" <> '')
);

-- Current-row uniqueness per (tenant, series, fixing date).
create unique index if not exists ores_marketdata_fixings_current_uniq_idx
on ores_marketdata_fixings_tbl (tenant_id, series_id, fixing_date)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Lookup by series + date range.
create index if not exists ores_marketdata_fixings_series_date_idx
on ores_marketdata_fixings_tbl (tenant_id, series_id, fixing_date desc);

-- Lookup by tenant across all series.
create index if not exists ores_marketdata_fixings_tenant_date_idx
on ores_marketdata_fixings_tbl (tenant_id, fixing_date desc);

-- Lookup by source.
create index if not exists ores_marketdata_fixings_source_idx
on ores_marketdata_fixings_tbl (tenant_id, source, fixing_date desc)
where source is not null;

-- =============================================================================
-- Insert trigger — soft-update pattern.
-- =============================================================================
create or replace function ores_marketdata_fixings_insert_fn()
returns trigger as $$
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    update ores_marketdata_fixings_tbl
    set valid_to = current_timestamp
    where tenant_id   = new.tenant_id
      and series_id   = new.series_id
      and fixing_date = new.fixing_date
      and valid_to    = ores_utility_infinity_timestamp_fn()
      and valid_from  < current_timestamp;

    new.valid_from := current_timestamp;
    new.valid_to   := ores_utility_infinity_timestamp_fn();
    return new;
end;
$$ language plpgsql;

create or replace trigger ores_marketdata_fixings_insert_trg
before insert on ores_marketdata_fixings_tbl
for each row execute function ores_marketdata_fixings_insert_fn();

-- =============================================================================
-- Soft-delete trigger — implements the soft-delete pattern for corrections.
-- Rules are incompatible with TimescaleDB hypertables; a BEFORE DELETE trigger
-- returning NULL suppresses the physical delete and closes the current row
-- instead, achieving the same semantics.
-- =============================================================================
create or replace function ores_marketdata_fixings_delete_fn()
returns trigger as $$
begin
    update ores_marketdata_fixings_tbl
    set valid_to = current_timestamp
    where tenant_id   = old.tenant_id
      and series_id   = old.series_id
      and fixing_date = old.fixing_date
      and valid_to    = ores_utility_infinity_timestamp_fn();
    return null;
end;
$$ language plpgsql;

create or replace trigger ores_marketdata_fixings_delete_trg
before delete on ores_marketdata_fixings_tbl
for each row execute function ores_marketdata_fixings_delete_fn();

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
        perform public.create_hypertable(
            'ores_marketdata_fixings_tbl',
            'fixing_date',
            chunk_time_interval => interval '30 days',
            if_not_exists => true
        );
        raise notice 'Created hypertable with 30-day chunks for ores_marketdata_fixings_tbl';
    end if;
end $$;
