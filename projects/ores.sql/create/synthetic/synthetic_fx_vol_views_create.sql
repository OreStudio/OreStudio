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
 * FX Volatility View Table
 *
 * A view is one vendor's quotes of a latent FX volatility surface: the surface,
 * which no vendor publishes as its own, with the vendor's noise over it. It is a
 * feed of the vendor's collection, named in the vendor's namespace, such as
 * synthetic.bloomberg.fx.fx_option.audusd, and listed, counted, stopped and
 * started with the vendor's other feeds. It reads two inputs: the latent surface
 * it quotes, and the vendor's noise profile it applies, which is one per vendor
 * and shared by every pair the vendor quotes. The vendor's simulated source runs
 * it, holding the shared surface and publishing the view's points under the
 * vendor's ids.
 *
 * A view of the clean demo contributor applies no noise: it records the latent
 * surface itself, for the tests to compare against.
 */

create table if not exists "ores_synthetic_fx_vol_views_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "feed_id" uuid not null,
    "contributor_id" uuid not null,
    "config_id" uuid not null,
    "folder_id" uuid null,
    "source_name" text not null,
    "enabled" boolean not null,
    "auto_start" boolean not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "acting_agent" text not null default '',
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
    check ("source_name" like 'synthetic.%')
);

-- Composite natural key: unique combination for active records
create unique index if not exists fx_vol_views_party_id_feed_id_contributor_id_uniq_idx
on "ores_synthetic_fx_vol_views_tbl" (tenant_id, party_id, feed_id, contributor_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists fx_vol_views_version_uniq_idx
on "ores_synthetic_fx_vol_views_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists fx_vol_views_id_uniq_idx
on "ores_synthetic_fx_vol_views_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists fx_vol_views_tenant_idx
on "ores_synthetic_fx_vol_views_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_synthetic_fx_vol_views_insert_fn()
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

    -- Validate feed_id (soft FK to ores_synthetic_fx_vol_surface_feeds_tbl)
    if not exists (
        select 1 from ores_synthetic_fx_vol_surface_feeds_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.feed_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid feed_id: %. No active fx_vol_surface_feed found with this id.', NEW.feed_id
            using errcode = '23503';
    end if;

    -- Validate contributor_id (soft FK to ores_synthetic_fx_vol_contributors_tbl)
    if not exists (
        select 1 from ores_synthetic_fx_vol_contributors_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.contributor_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid contributor_id: %. No active fx_vol_contributor found with this id.', NEW.contributor_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_synthetic_fx_vol_views_tbl"
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
                    'fx_vol_view',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'fx_vol_view',
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
        update "ores_synthetic_fx_vol_views_tbl"
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
    NEW.acting_agent = coalesce(ores_iam_current_agent_fn(), '');

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_synthetic_fx_vol_views_insert_trg
before insert on "ores_synthetic_fx_vol_views_tbl"
for each row execute function ores_synthetic_fx_vol_views_insert_fn();

create or replace rule ores_synthetic_fx_vol_views_delete_rule as
on delete to "ores_synthetic_fx_vol_views_tbl" do instead (
    update "ores_synthetic_fx_vol_views_tbl"
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
