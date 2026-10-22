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
 * Market Data Source Table
 *
 * A source is one adapter instance of a provider: bloomberg-live and
 * bloomberg-sim are two sources of the provider bloomberg. Its kind says how the
 * adapter gets its values and is data a person can read. No code in the pipeline
 * branches on it. The name is a label and no code parses it.
 *
 * A provider can have several sources at once. A party reaches one of them through a
 * route, never through the provider. One source of each provider is the default, and a
 * new subscription routes to it.
 *
 * The system tenant and the system party own the rows. Every tenant reads them and
 * only the system tenant writes them.
 */

create table if not exists "ores_marketdata_market_data_sources_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "provider_code" text not null,
    "kind" text not null,
    "is_default" boolean not null,
    "description" text not null,
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
    check ("name" ~ '^[a-z][a-z0-9_-]*$'),
    check ("provider_code" <> '')
);

-- Unique name for active records
create unique index if not exists market_data_sources_name_uniq_idx
on "ores_marketdata_market_data_sources_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists market_data_sources_version_uniq_idx
on "ores_marketdata_market_data_sources_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists market_data_sources_id_uniq_idx
on "ores_marketdata_market_data_sources_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists market_data_sources_tenant_idx
on "ores_marketdata_market_data_sources_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists market_data_sources_provider_idx
on "ores_marketdata_market_data_sources_tbl" (tenant_id, provider_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists market_data_sources_default_idx
on "ores_marketdata_market_data_sources_tbl" (tenant_id, provider_code)
where valid_to = ores_utility_infinity_timestamp_fn()
  and "is_default" = true;

create or replace function ores_marketdata_market_data_sources_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate kind
    NEW.kind := ores_refdata_validate_source_kind_fn(NEW.tenant_id, NEW.kind);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_marketdata_market_data_sources_tbl"
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
                    'market_data_source',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'market_data_source',
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
        update "ores_marketdata_market_data_sources_tbl"
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

create or replace trigger ores_marketdata_market_data_sources_insert_trg
before insert on "ores_marketdata_market_data_sources_tbl"
for each row execute function ores_marketdata_market_data_sources_insert_fn();

create or replace rule ores_marketdata_market_data_sources_delete_rule as
on delete to "ores_marketdata_market_data_sources_tbl" do instead (
    update "ores_marketdata_market_data_sources_tbl"
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
-- Row-level security: tenant isolation for Market Data Source
-- Every session reads the system tenant's own rows; writes stay the
-- current tenant's.
-- =============================================================================
alter table ores_marketdata_market_data_sources_tbl enable row level security;

drop policy if exists market_data_sources_tbl_tenant_isolation_policy
    on ores_marketdata_market_data_sources_tbl;

create policy market_data_sources_tbl_tenant_isolation_policy
on ores_marketdata_market_data_sources_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
