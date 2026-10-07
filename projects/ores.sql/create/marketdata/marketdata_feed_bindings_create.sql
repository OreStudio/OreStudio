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
 * A feed binding records that a party consumes one producer source. It is the
 * one place a live tick finds its owners, for every feed and every asset class: a
 * tick names its own datum by its oresmd quote URI and its producer by source,
 * and the ingest loop stores it once for each enabled binding of that source,
 * under the binding's tenant and party, and republishes it on the per-party
 * realtime stream marketdata.v1.ops.market_tick.<tenant_id>.<party_id>.<ore_key>, where the
 * key is the datum's canonical ORE key. Bindings are created by provisioning
 * against the system party's config, not per office: each party consumes the
 * shared stream into its own per-party series.
 *
 * Rebinding (editing source_name) switches the ingest source without restarting
 * producers. Setting enabled  false= suspends the subscription without deleting
 * the binding.
 *
 * A binding also says what kind of producer it names: a real feed (VENDOR) or a
 * generated one (SYNTHETIC). The ingest loop stamps the series it creates with
 * the binding's producer_kind, so a reader of a series can tell generated data
 * from observed data without guessing from the source string. The column
 * defaults to VENDOR, so every binding a real feed creates carries it without
 * setting anything.
 *
 * This model binds to no variability profile. Its features match
 * uuid-surrogate-lookup, but that profile also enables the shell command
 * facet, which feed bindings do not have today. Binding it is a decision about
 * the shell surface, not about the model.
 */

create table if not exists "ores_marketdata_feed_bindings_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "source_name" text not null,
    "producer_kind" text not null,
    "enabled" boolean not null,
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
    check ("source_name" <> '')
);

-- Composite natural key: unique combination for active records
create unique index if not exists feed_bindings_party_id_source_name_uniq_idx
on "ores_marketdata_feed_bindings_tbl" (tenant_id, party_id, source_name)
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

create or replace function ores_marketdata_feed_bindings_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate producer_kind
    NEW.producer_kind := ores_refdata_validate_producer_kind_fn(NEW.tenant_id, NEW.producer_kind);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_marketdata_feed_bindings_tbl"
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
                    'feed_binding',
                    'id',
                    NEW.id::text);
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'feed_binding',
                'id',
                NEW.id::text,
                NEW.version::text,
                current_version::text);
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

-- =============================================================================
-- Row-level security: tenant isolation for Feed Binding
-- System-tenant sessions may also read every tenant's rows.
-- =============================================================================
alter table ores_marketdata_feed_bindings_tbl enable row level security;

drop policy if exists feed_bindings_tbl_tenant_isolation_policy
    on ores_marketdata_feed_bindings_tbl;

create policy feed_bindings_tbl_tenant_isolation_policy
on ores_marketdata_feed_bindings_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
);
