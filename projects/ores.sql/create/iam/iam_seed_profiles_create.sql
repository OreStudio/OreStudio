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
 * Seed Profile Table
 *
 * A seed profile is the data set that provisioning gives a new tenant. One
 * row orders step kinds from the catalogue in code, declares the parameters
 * its form takes, carries the tenant details it prefills, and carries the card
 * the person choosing a starting point reads.
 *
 * A new profile is a row, so an operator adds one with no code change. A new
 * step kind is code. The accepted seed profile contract fixes both lists,
 * and the first two rows are:
 *
 * - empty_operational: the production starting point. It publishes the
 *   base bundle, takes a counterparty count and a GLEIF root LEI to
 *   import, and creates no test data.
 * - acme_demo: the Acme Corporation holding group, with its staff, books
 *   and market data. Every test datum lives here.
 *
 * A profile is system-owned registered data. The system_scope flag states
 * that the table stores it under the system tenant and that the insert
 * trigger forces that tenant. Provisioning reads a profile before the tenant
 * it creates exists, so a profile cannot belong to the tenant being made.
 *
 * The profile the model binds is uuid-identified-lookup, the one its
 * sibling ores.iam.tenant binds, because that profile leaves
 * system_scope to the model. uuid-surrogate-lookup fixes it to false,
 * which a row the platform owns cannot be.
 *
 * The ordered steps are rows of ores.iam.seed_profile_step, and the form
 * schema is rows of ores.iam.seed_profile_parameter. Both are children of
 * this entity.
 */

create table if not exists "ores_iam_seed_profiles_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null default ores_utility_system_tenant_id_fn(),
    "version" integer not null,
    "code" text not null,
    "name" text not null,
    "summary" text not null,
    "audience" text not null,
    "bullets_json" jsonb not null default '[]'::jsonb,
    "tenant_type" text not null default 'production',
    "tenant_name" text not null default '',
    "tenant_code" text not null default '',
    "tenant_hostname" text null,
    "admin_username" text not null default '',
    "admin_email" text not null default '',
    "inherits_admin_password" boolean not null default false,
    "force_password_change" boolean not null default false,
    "display_order" integer not null default 0,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, valid_from, valid_to),
    exclude using gist (
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("code" <> ''),
    check ("name" <> '')
);

-- Unique code for active records
create unique index if not exists seed_profiles_code_uniq_idx
on "ores_iam_seed_profiles_tbl" (code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists seed_profiles_version_uniq_idx
on "ores_iam_seed_profiles_tbl" (id, version)
where valid_to = ores_utility_infinity_timestamp_fn();


create or replace function ores_iam_seed_profiles_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- All seed_profiles belong to the system tenant
    NEW.tenant_id := ores_utility_system_tenant_id_fn();

    -- Validate tenant_type
    NEW.tenant_type := ores_iam_validate_tenant_type_fn(NEW.tenant_id, NEW.tenant_type);

    -- Validate change_reason_code (use system tenant for seed_profiles records)
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(ores_utility_system_tenant_id_fn(), NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_iam_seed_profiles_tbl"
    where id = NEW.id
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
                    'seed_profile',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'seed_profile',
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
        update "ores_iam_seed_profiles_tbl"
        set valid_to = clock_timestamp()
        where id = NEW.id
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

create or replace trigger ores_iam_seed_profiles_insert_trg
before insert on "ores_iam_seed_profiles_tbl"
for each row execute function ores_iam_seed_profiles_insert_fn();

create or replace rule ores_iam_seed_profiles_delete_rule as
on delete to "ores_iam_seed_profiles_tbl" do instead (
    update "ores_iam_seed_profiles_tbl"
    set valid_to = clock_timestamp()
    where id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
