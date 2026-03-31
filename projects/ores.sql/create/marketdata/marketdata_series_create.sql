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
-- Market data series catalog.
-- Identifies what is being observed: a yield curve, vol surface, spot rate,
-- fixing index, etc. Standard temporal table (not TimescaleDB) — this is
-- reference/catalog data that changes infrequently. GIST exclusion is fine here.
--
-- Key decomposition: every ORE market data key follows the skeleton
--   TYPE / METRIC / [QUALIFIER...] / [POINT_ID]
-- The qualifier and point_id split is type-specific (registry in C++); the DB
-- stores both as free text. The full ORE key is always reconstructable.
--
-- Asset class + series_subclass taxonomy:
--   FX:          SPOT (FX), FORWARD (FXFWD), VOLATILITY (FX_OPTION)
--   RATES:       YIELD (MM/DISCOUNT/ZERO/IR_SWAP), VOLATILITY (SWAPTION/CAPFLOOR),
--                BASIS (BASIS_SWAP/BMA_SWAP), FRA (FRA/IMM_FRA/MM_FUTURE),
--                XCCY (CC_BASIS_SWAP/CC_FIX_FLOAT_SWAP)
--   CREDIT:      SPREAD (HAZARD_RATE/CDS), INDEX (CDS_INDEX/INDEX_CDS_OPTION),
--                RECOVERY (RECOVERY_RATE)
--   EQUITY:      SPOT (EQUITY), FORWARD (EQUITY_FWD/EQUITY_DIVIDEND),
--                VOLATILITY (EQUITY_OPTION)
--   COMMODITY:   SPOT (COMMODITY), FORWARD (COMMODITY_FWD),
--                VOLATILITY (COMMODITY_OPTION)
--   INFLATION:   SWAP (ZC/YY_INFLATIONSWAP), CAPFLOOR (ZC/YY_INFLATIONCAPFLOOR),
--                SEASONALITY (SEASONALITY)
--   BOND:        PRICE (BOND/PRICE), SPREAD (BOND/YIELD_SPREAD)
--   CROSS_ASSET: CORRELATION (CORRELATION)
-- =============================================================================

create table if not exists ores_marketdata_series_tbl (
    "id"               uuid not null,
    "tenant_id"        uuid not null,
    "version"          integer not null,
    "series_type"      text not null,
    "metric"           text not null,
    "qualifier"        text not null,
    "asset_class"      text not null,
    "series_subclass"  text not null,
    "is_scalar"        boolean not null default false,
    "modified_by"      text not null,
    "change_reason_code" text not null,
    "change_commentary"  text not null,
    "valid_from"       timestamp with time zone not null,
    "valid_to"         timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    exclude using gist (
        tenant_id with =,
        id with =,
        tstzrange(valid_from, valid_to) with &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("series_type" <> ''),
    check ("metric" <> ''),
    check ("qualifier" <> ''),
    check ("asset_class" <> ''),
    check ("series_subclass" <> '')
);

-- At most one current row per (tenant, type, metric, qualifier) triple —
-- prevents duplicate series registrations.
create unique index if not exists ores_marketdata_series_natural_key_uniq_idx
on ores_marketdata_series_tbl (tenant_id, series_type, metric, qualifier)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_marketdata_series_version_uniq_idx
on ores_marketdata_series_tbl (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_marketdata_series_tenant_idx
on ores_marketdata_series_tbl (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_marketdata_series_asset_class_idx
on ores_marketdata_series_tbl (tenant_id, asset_class, series_subclass)
where valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Insert trigger — standard upsert-by-insert with optimistic concurrency.
-- =============================================================================
create or replace function ores_marketdata_series_insert_fn()
returns trigger
security definer
set search_path = public
as $$
declare
    current_version integer;
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    select version into current_version
    from ores_marketdata_series_tbl
    where tenant_id = new.tenant_id
      and id        = new.id
      and valid_to  = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update ores_marketdata_series_tbl
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
          and id        = new.id
          and valid_to  = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to   = ores_utility_infinity_timestamp_fn();

    new.change_reason_code := ores_dq_validate_change_reason_fn(
        new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_marketdata_series_insert_trg
before insert on ores_marketdata_series_tbl
for each row
execute function ores_marketdata_series_insert_fn();

-- =============================================================================
-- Soft-delete rule.
-- =============================================================================
create or replace rule ores_marketdata_series_delete_rule as
on delete to ores_marketdata_series_tbl do instead
    update ores_marketdata_series_tbl
    set valid_to = current_timestamp
    where tenant_id = old.tenant_id
      and id        = old.id
      and valid_to  = ores_utility_infinity_timestamp_fn();
