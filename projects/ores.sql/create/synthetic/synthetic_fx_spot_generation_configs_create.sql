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
 * FX Spot Generation Config Table
 *
 * A typed sub-configuration owned by a market_data_generation_config. Describes
 * how to synthesise the tick stream for a single FX spot rate: which ORE market
 * key it produces, the starting price, the tick cadence, and the price process.
 * The price process is a Gaussian Mixture Model whose components are held
 * separately as gmm_component rows. Scoped to a tenant and a party.
 */

create table if not exists "ores_synthetic_fx_spot_generation_configs_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "config_id" uuid not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "source_name" text not null,
    "ore_key" text not null,
    "gmm_initial_price" double precision not null,
    "ticks_per_hour" integer not null,
    "process_type" text not null,
    "enabled" boolean not null,
    "vintage_source" text not null,
    "vintage_date" date not null,
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
    check ("base_currency_code" <> ''),
    check ("quote_currency_code" <> ''),
    check ("base_currency_code" <> "quote_currency_code"),
    check ("source_name" <> ''),
    check ("ore_key" <> ''),
    check ("gmm_initial_price" > 0),
    check ("ticks_per_hour" > 0),
    check ("process_type" in ('geometric', 'arithmetic', 'ou')),
    check ("vintage_source" <> ''),
    check ("vintage_date" is not null)
);

-- Composite natural key: unique combination for active records
create unique index if not exists fx_spot_generation_configs_party_id_config_id_base_currency_code_quote_currency_code_uniq_idx
on "ores_synthetic_fx_spot_generation_configs_tbl" (tenant_id, party_id, config_id, base_currency_code, quote_currency_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists fx_spot_generation_configs_version_uniq_idx
on "ores_synthetic_fx_spot_generation_configs_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists fx_spot_generation_configs_id_uniq_idx
on "ores_synthetic_fx_spot_generation_configs_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists fx_spot_generation_configs_tenant_idx
on "ores_synthetic_fx_spot_generation_configs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_synthetic_fx_spot_generation_configs_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate config_id (soft FK to ores_synthetic_market_data_generation_configs_tbl)
    if not exists (
        select 1 from ores_synthetic_market_data_generation_configs_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.config_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid config_id: %. No active market_data_generation_config found with this id.', NEW.config_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_synthetic_fx_spot_generation_configs_tbl"
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

        update "ores_synthetic_fx_spot_generation_configs_tbl"
        set valid_to = current_timestamp
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_synthetic_fx_spot_generation_configs_insert_trg
before insert on "ores_synthetic_fx_spot_generation_configs_tbl"
for each row execute function ores_synthetic_fx_spot_generation_configs_insert_fn();

create or replace rule ores_synthetic_fx_spot_generation_configs_delete_rule as
on delete to "ores_synthetic_fx_spot_generation_configs_tbl" do instead
    update "ores_synthetic_fx_spot_generation_configs_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Validation function for fx_spot_generation_config
-- Validates that a id exists in the fx_spot_generation_configs table.
-- Returns the validated value, or default if null/empty.
-- Validates against both the tenant's own data and the system tenant's canonical set.
-- =============================================================================
create or replace function ores_synthetic_validate_fx_spot_generation_config_fn(
    p_tenant_id uuid,
    p_value uuid
) returns uuid as $$
begin
    -- Return default if null or empty
    if p_value is null then
        raise exception 'Invalid fx_spot_generation_config: value cannot be null'
            using errcode = '23502';
    end if;

    -- Allow pass-through if neither this tenant nor the system tenant has
    -- seeded active fx_spot_generation_configs yet (freshly provisioned tenant).
    if not exists (
        select 1 from ores_synthetic_fx_spot_generation_configs_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    -- Validate against this tenant's values and the system tenant's canonical set.
    if not exists (
        select 1 from ores_synthetic_fx_spot_generation_configs_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid fx_spot_generation_config: %. Must be one of: %', p_value, (
            select string_agg(id::text, ', ' order by id)
            from ores_synthetic_fx_spot_generation_configs_tbl
            where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
