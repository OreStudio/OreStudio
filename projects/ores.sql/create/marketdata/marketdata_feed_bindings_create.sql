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
 * Feed Binding Table
 *
 * A feed binding records which raw producer channel feeds an official market
 * series, in which workspace. The marketdata service reads all enabled bindings
 * at startup, subscribes to synthetic.v1.tick.fx_spot.<source_name> once per
 * (tenant, party, workspace), persists each arriving tick as a
 * market_observation under the binding's party, and republishes on the
 * per-party realtime stream
 * marketdata.v1.tick.<tenant_id>.<workspace_id>.<party_id>.<ore_key>.
 *
 * workspace_id defaults to the Live sentinel (aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa),
 * which resolves in every tenant. It is the seam where the future workspaces
 * feature binds scenario data: a binding reads "party P consumes source S in
 * workspace W" (see
 * [[file:../../../doc/llm/specs/simulated-market-data-strategy.allium][simulated-market-data-strategy.allium]]).
 * Bindings are created by provisioning against the system party's config, not
 * per office: each party consumes the shared stream into its own per-party
 * series.
 *
 * Rebinding (editing source_name) switches the ingest source without restarting
 * producers. Setting enabled  false= suspends the subscription without deleting
 * the binding.
 */

create table if not exists "ores_marketdata_feed_bindings_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "ore_key" text not null,
    "source_name" text not null,
    "asset_class" text not null,
    "enabled" boolean not null,
    "workspace_id" uuid not null default ores_utility_live_workspace_id_fn(), -- soft FK to ores_workspaces_tbl(id)
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
    check ("ore_key" <> ''),
    check ("source_name" <> ''),
    check ("asset_class" <> '')
);

-- Composite natural key: unique combination for active records
create unique index if not exists feed_bindings_party_id_ore_key_source_name_uniq_idx
on "ores_marketdata_feed_bindings_tbl" (tenant_id, party_id, ore_key, source_name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists feed_bindings_version_uniq_idx
on "ores_marketdata_feed_bindings_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists feed_bindings_id_uniq_idx
on "ores_marketdata_feed_bindings_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists feed_bindings_tenant_idx
on "ores_marketdata_feed_bindings_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists feed_bindings_workspace_idx
on "ores_marketdata_feed_bindings_tbl" (workspace_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_marketdata_feed_bindings_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate workspace_id
    NEW.workspace_id := ores_workspace_validate_fn(NEW.workspace_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_marketdata_feed_bindings_tbl"
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
        update "ores_marketdata_feed_bindings_tbl"
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

create or replace trigger ores_marketdata_feed_bindings_insert_trg
before insert on "ores_marketdata_feed_bindings_tbl"
for each row execute function ores_marketdata_feed_bindings_insert_fn();

create or replace rule ores_marketdata_feed_bindings_delete_rule as
on delete to "ores_marketdata_feed_bindings_tbl" do instead (
    update "ores_marketdata_feed_bindings_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
