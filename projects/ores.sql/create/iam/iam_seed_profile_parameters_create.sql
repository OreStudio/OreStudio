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
 * Seed Profile Parameter Table
 *
 * The schema of the form that a seed profile presents: one row per
 * parameter, stating the label the form shows, the key a shell command types,
 * the type, the default and whether a value is required. The web builds the
 * tenant form from these rows. The shell takes the same values as
 * =--param key=value=.
 *
 * The declared shape is deliberately small. A profile decides how many
 * counterparties to create and which GLEIF root LEI to import; it does not
 * decide what a step kind does, because that is code.
 *
 * A parameter whose type is choice names the values it accepts, in the order
 * the form offers them. A choice is declared rather than left to free text so
 * that a value the run cannot use is not typeable at all.
 *
 * A profile with no parameters states that its form asks for nothing. The
 * demonstration profile is one: it carries every value it needs, so its
 * journey presents no input at all.
 *
 * The pair (seed_profile_id, name) is the identity.
 */

create table if not exists "ores_iam_seed_profile_parameters_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null default ores_utility_system_tenant_id_fn(),
    "version" integer not null,
    "seed_profile_id" uuid not null,
    "name" text not null,
    "label" text not null,
    "data_type" text not null,
    "choices_json" jsonb null,
    "default_value" text null,
    "is_required" boolean not null default true,
    "description" text not null default '',
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
    check ("name" <> ''),
    check ("data_type" <> '')
);

-- Composite natural key: unique combination for active records
create unique index if not exists seed_profile_parameters_seed_profile_id_name_uniq_idx
on "ores_iam_seed_profile_parameters_tbl" (tenant_id, seed_profile_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists seed_profile_parameters_version_uniq_idx
on "ores_iam_seed_profile_parameters_tbl" (id, version)
where valid_to = ores_utility_infinity_timestamp_fn();


create or replace function ores_iam_seed_profile_parameters_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- All seed_profile_parameters belong to the system tenant
    NEW.tenant_id := ores_utility_system_tenant_id_fn();

    -- Validate seed_profile_id (soft FK to ores_iam_seed_profiles_tbl)
    if not exists (
        select 1 from ores_iam_seed_profiles_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.seed_profile_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid seed_profile_id: %. No active seed profile found with this id.', NEW.seed_profile_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code (use system tenant for seed_profile_parameters records)
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(ores_utility_system_tenant_id_fn(), NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_iam_seed_profile_parameters_tbl"
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
                    'seed_profile_parameter',
                    'id',
                    NEW.id::text);
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'seed_profile_parameter',
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
        update "ores_iam_seed_profile_parameters_tbl"
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

create or replace trigger ores_iam_seed_profile_parameters_insert_trg
before insert on "ores_iam_seed_profile_parameters_tbl"
for each row execute function ores_iam_seed_profile_parameters_insert_fn();

create or replace rule ores_iam_seed_profile_parameters_delete_rule as
on delete to "ores_iam_seed_profile_parameters_tbl" do instead (
    update "ores_iam_seed_profile_parameters_tbl"
    set valid_to = clock_timestamp()
    where id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
