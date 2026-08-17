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
 * Market Series Table
 *
 * A catalog entry for a market data series — it records what is being observed:
 * a yield curve, vol surface, spot rate, fixing index, or similar. Standard
 * temporal reference data; changes infrequently so a regular table with GIST
 * exclusion is appropriate.
 *
 * Identity is the canonical oresmd URI (e.g.
 * oresmd://ir/eur?tenor3m&typequote&quoteir_swap&metricrate&point5y).
 * Classification (asset class, subclass, scalar-ness) derives from the URI in
 * the oresmd layer, not from stored columns. The URI is read and written
 * end-to-end: the import boundaries project ORE keys into URIs via
 * oresmd_projections::from_ore_key= / from_index_name, and the oresmd
 * parser canonicalises before persistence.
 *
 * derivation_kind/derivation_config_id/derivation_config_version mark
 * whether this series is directly observed (the sentinel OBSERVED) or
 * derived by a named mechanism (e.g. IR_CURVE_BOOTSTRAP,
 * CRM_DERIVATION) -- so any published series answers "was this observed
 * or computed, and by what" without guessing from the source tag on its
 * observations. Per-observation lineage (which source series/as-of a
 * specific derived point came from) is a separate concern, tracked by
 * observation_lineage, not this catalog-level marker.
 */

create table if not exists "ores_marketdata_market_series_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "oresmd_uri" text not null,
    "derivation_kind" text not null,
    "derivation_config_id" uuid not null,
    "derivation_config_version" integer not null default 0,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("oresmd_uri" <> ''),
    check (("derivation_kind" = 'OBSERVED' and "derivation_config_id" = ores_utility_nil_uuid_fn() and "derivation_config_version" = 0) or ("derivation_kind" <> 'OBSERVED' and "derivation_config_id" <> ores_utility_nil_uuid_fn() and "derivation_config_version" <> 0))
);

-- Composite natural key: unique combination for active records
create unique index if not exists market_series_party_id_oresmd_uri_uniq_idx
on "ores_marketdata_market_series_tbl" (tenant_id, party_id, oresmd_uri)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists market_series_version_uniq_idx
on "ores_marketdata_market_series_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists market_series_id_uniq_idx
on "ores_marketdata_market_series_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists market_series_tenant_idx
on "ores_marketdata_market_series_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_marketdata_market_series_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate derivation_kind
    NEW.derivation_kind := ores_refdata_validate_derivation_kind_fn(NEW.tenant_id, NEW.derivation_kind);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_marketdata_market_series_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_marketdata_market_series_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_market_series_insert_trg
before insert on "ores_marketdata_market_series_tbl"
for each row execute function ores_marketdata_market_series_insert_fn();

create or replace rule ores_marketdata_market_series_delete_rule as
on delete to "ores_marketdata_market_series_tbl" do instead (
    update "ores_marketdata_market_series_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
