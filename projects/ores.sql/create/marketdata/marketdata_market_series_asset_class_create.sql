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
 * Template: sql_schema_junction_create.mustache
 * To modify, update the template and regenerate.
 *
 * Market Series Asset Class Table
 *
 * A market series is a container of observations, and the asset classes it
 * serves follow from what it observes rather than from a single value on
 * the series itself. Most series belong to exactly one class, but a
 * pairwise correlation relates two -- an equity index against an FX rate,
 * say -- and belongs to both. A not-null column cannot hold that, so the
 * relationship is a junction.
 */

create table if not exists "ores_marketdata_market_series_asset_classes_tbl" (
    "market_series_id" uuid not null,
    "tenant_id" uuid not null,
    "asset_class_code" text not null,
    "version" integer not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, market_series_id, asset_class_code, valid_from),
    exclude using gist (
        tenant_id WITH =,
        market_series_id WITH =,
        asset_class_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for looking up the classes a series belongs to
create index if not exists market_series_asset_classes_series_idx
on "ores_marketdata_market_series_asset_classes_tbl" (market_series_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding the series in an asset class
create index if not exists market_series_asset_classes_asset_class_idx
on "ores_marketdata_market_series_asset_classes_tbl" (asset_class_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists market_series_asset_classes_uniq_idx
on "ores_marketdata_market_series_asset_classes_tbl" (tenant_id, market_series_id, asset_class_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists market_series_asset_classes_tenant_idx
on "ores_marketdata_market_series_asset_classes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_marketdata_market_series_asset_classes_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Version management
    select version into current_version
    from "ores_marketdata_market_series_asset_classes_tbl"
    where tenant_id = new.tenant_id
    and market_series_id = new.market_series_id
    and asset_class_code = new.asset_class_code
    and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        -- Close existing record.
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. the same junction pair touched
        -- twice in one transaction) would collide with itself.
        -- clock_timestamp() always advances.
        update "ores_marketdata_market_series_asset_classes_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = new.tenant_id
        and market_series_id = new.market_series_id
        and asset_class_code = new.asset_class_code
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < clock_timestamp();
    else
        new.version = 1;
    end if;

    new.valid_from = clock_timestamp();
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    -- Validate market_series_id (soft FK to ores_marketdata_market_series_tbl)
    if not exists (
        select 1 from ores_marketdata_market_series_tbl
        where tenant_id = new.tenant_id
          and id = new.market_series_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid market_series_id: %. No market series found with this id.', new.market_series_id
            using errcode = '23503';
    end if;

    -- Validate asset_class_code
    new.asset_class_code := ores_refdata_validate_asset_class_code_fn(new.tenant_id, new.asset_class_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_marketdata_market_series_asset_classes_insert_trg
before insert on "ores_marketdata_market_series_asset_classes_tbl"
for each row
execute function ores_marketdata_market_series_asset_classes_insert_fn();

create or replace rule ores_marketdata_market_series_asset_classes_delete_rule as
on delete to "ores_marketdata_market_series_asset_classes_tbl"
do instead
  update "ores_marketdata_market_series_asset_classes_tbl"
  set valid_to = clock_timestamp()
  where tenant_id = old.tenant_id
  and market_series_id = old.market_series_id
  and asset_class_code = old.asset_class_code
  and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Row-level security: tenant isolation for Market Series Asset Class
-- =============================================================================
alter table "ores_marketdata_market_series_asset_classes_tbl" enable row level security;

create policy market_series_asset_classes_tbl_tenant_isolation_policy
on "ores_marketdata_market_series_asset_classes_tbl"
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
