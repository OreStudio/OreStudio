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
 * FX Volatility Surface Feed Table
 *
 * A synthetic volatility surface of one currency pair, as
 * [[id:D4C7C4A7-CD2D-4307-AB06-DA9BE3C3A334][A synthetic contributor publishes noisy observations of a latent surface]]
 * describes. The surface is SSVI with gamma fixed at one half. Its state is the
 * forward variance of each segment between the feed's expiries, which the
 * fx_vol_surface_feed_expiry rows hold, a skew driver and a curvature driver,
 * each reverting to where the surface started.
 *
 * The correlation of the shocks is three numbers, and the feed builds the matrix
 * from them: spot with every forward variance, spot with the skew, and how strongly
 * neighbouring forward variances move together, which falls by that factor for each
 * segment between them. Every other shock is independent of spot. The surface is
 * quoted by the contributors fx_vol_surface_feed_contributor names.
 */

create table if not exists "ores_synthetic_fx_vol_surface_feeds_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "config_id" uuid not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "source_name" text not null,
    "ticks_per_hour" integer not null,
    "enabled" boolean not null,
    "auto_start" boolean not null,
    "folder_id" uuid null,
    "asset_class_code" text not null,
    "arrival_mode" text not null default 'periodic',
    "branching_ratio" double precision not null default 0.0,
    "decay" double precision not null default 1.0,
    "sensitivity" double precision not null default 1.0,
    "skew_reversion" double precision not null,
    "skew_volatility" double precision not null,
    "curvature_reversion" double precision not null,
    "curvature_volatility" double precision not null,
    "spot_variance_correlation" double precision not null,
    "spot_skew_correlation" double precision not null,
    "segment_correlation" double precision not null,
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
    check ("base_currency_code" <> "quote_currency_code"),
    check ("source_name" like 'synthetic.%'),
    check ("ticks_per_hour" > 0),
    check ("arrival_mode" in ('periodic', 'poisson', 'hawkes')),
    check ("skew_reversion" >= 0 and "skew_reversion" < 'Infinity'),
    check ("skew_volatility" >= 0 and "skew_volatility" < 'Infinity'),
    check ("curvature_reversion" >= 0 and "curvature_reversion" < 'Infinity'),
    check ("curvature_volatility" >= 0 and "curvature_volatility" < 'Infinity'),
    check ("spot_variance_correlation" > -1 and "spot_variance_correlation" < 1),
    check ("spot_skew_correlation" > -1 and "spot_skew_correlation" < 1),
    check ("segment_correlation" >= 0 and "segment_correlation" < 1)
);

-- Composite natural key: unique combination for active records
create unique index if not exists fx_vol_surface_feeds_party_id_config_id_base_currency_code_quote_currency_code_uniq_idx
on "ores_synthetic_fx_vol_surface_feeds_tbl" (tenant_id, party_id, config_id, base_currency_code, quote_currency_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists fx_vol_surface_feeds_version_uniq_idx
on "ores_synthetic_fx_vol_surface_feeds_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists fx_vol_surface_feeds_id_uniq_idx
on "ores_synthetic_fx_vol_surface_feeds_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists fx_vol_surface_feeds_tenant_idx
on "ores_synthetic_fx_vol_surface_feeds_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_synthetic_fx_vol_surface_feeds_insert_fn()
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

    -- Validate folder_id (optional soft FK to ores_synthetic_folders_tbl)
    if NEW.folder_id is not null then
        if not exists (
            select 1 from ores_synthetic_folders_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.folder_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid folder_id: %. No active folder found with this id.', NEW.folder_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate base_currency_code
    NEW.base_currency_code := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.base_currency_code);

    -- Validate quote_currency_code
    NEW.quote_currency_code := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.quote_currency_code);

    -- Validate asset_class_code
    NEW.asset_class_code := ores_refdata_validate_asset_class_code_fn(NEW.tenant_id, NEW.asset_class_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_synthetic_fx_vol_surface_feeds_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        -- The write states what it believes about the row, and the store is
        -- what decides. Version zero means one thing: no current row exists.
        -- So a create that collides with a live row is refused here, for every
        -- client, rather than by a check each client has to remember.
        if NEW.version = 0 then
            if not ores_utility_version_replace_allowed_fn() then
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'fx_vol_surface_feed',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'fx_vol_surface_feed',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_synthetic_fx_vol_surface_feeds_tbl"
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
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_synthetic_fx_vol_surface_feeds_insert_trg
before insert on "ores_synthetic_fx_vol_surface_feeds_tbl"
for each row execute function ores_synthetic_fx_vol_surface_feeds_insert_fn();

create or replace rule ores_synthetic_fx_vol_surface_feeds_delete_rule as
on delete to "ores_synthetic_fx_vol_surface_feeds_tbl" do instead (
    update "ores_synthetic_fx_vol_surface_feeds_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      -- A tenant reads the shared rows of the system tenant, so OLD can be one.
      -- The rule runs with the owner's rights, which row-level security does
      -- not bind, so the rule states the own-tenant limit that the delete
      -- policy cannot enforce here.
      and tenant_id = ores_iam_current_tenant_id_fn()
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Validation function for fx_vol_surface_feed
-- Validates that a id exists in the fx_vol_surface_feeds table.
-- Returns the validated value, or default if null/empty.
-- Validates against both the tenant's own data and the system tenant's canonical set.
-- =============================================================================
create or replace function ores_synthetic_validate_fx_vol_surface_feed_fn(
    p_tenant_id uuid,
    p_value uuid
) returns uuid as $$
begin
    -- Return default if null or empty
    if p_value is null then
        perform ores_outcome_raise_fn('missing_field', 'fx_vol_surface_feed');
    end if;

    -- Allow pass-through if neither this tenant nor the system tenant has
    -- seeded active fx_vol_surface_feeds yet (freshly provisioned tenant).
    if not exists (
        select 1 from ores_synthetic_fx_vol_surface_feeds_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    -- Validate against this tenant's values and the system tenant's canonical set.
    if not exists (
        select 1 from ores_synthetic_fx_vol_surface_feeds_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid fx_vol_surface_feed: %. Must be one of: %', p_value, (
            select string_agg(id::text, ', ' order by id)
            from ores_synthetic_fx_vol_surface_feeds_tbl
            where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
