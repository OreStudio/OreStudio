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
 * Seed Profile Step Table
 *
 * A seed profile's steps, in the order provisioning runs them. The row names
 * a step kind from the catalogue in code and supplies the arguments that
 * kind consumes, so a profile states the same step with different bundles
 * without a code change.
 *
 * The catalogue is fixed: publish_bundle, import_lei_hierarchy,
 * provision_party, load_staff, attach_photos and
 * start_market_feeds. A step kind that is not in the catalogue is a code
 * change, because one new kind means one new step handler.
 *
 * The pair (seed_profile_id, step_kind) is the identity: a profile runs a
 * kind once. Steps are idempotent, and a failed step stops the provisioned
 * instance at that step.
 */

create table if not exists "ores_iam_seed_profile_steps_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null default ores_utility_system_tenant_id_fn(),
    "version" integer not null,
    "seed_profile_id" uuid not null,
    "step_kind" text not null,
    "arguments_json" jsonb not null,
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
    check ("step_kind" <> '')
);

-- Composite natural key: unique combination for active records
create unique index if not exists seed_profile_steps_seed_profile_id_step_kind_uniq_idx
on "ores_iam_seed_profile_steps_tbl" (tenant_id, seed_profile_id, step_kind)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists seed_profile_steps_version_uniq_idx
on "ores_iam_seed_profile_steps_tbl" (id, version)
where valid_to = ores_utility_infinity_timestamp_fn();


create or replace function ores_iam_seed_profile_steps_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- All seed_profile_steps belong to the system tenant
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

    -- Validate change_reason_code (use system tenant for seed_profile_steps records)
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(ores_utility_system_tenant_id_fn(), NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_iam_seed_profile_steps_tbl"
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
                raise exception
                    'Row already exists: a create cannot replace it. State the version you read to replace the row, or ask for a version replace.'
                    using errcode = '23505';
            end if;
        elsif NEW.version != current_version then
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
        update "ores_iam_seed_profile_steps_tbl"
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

create or replace trigger ores_iam_seed_profile_steps_insert_trg
before insert on "ores_iam_seed_profile_steps_tbl"
for each row execute function ores_iam_seed_profile_steps_insert_fn();

create or replace rule ores_iam_seed_profile_steps_delete_rule as
on delete to "ores_iam_seed_profile_steps_tbl" do instead (
    update "ores_iam_seed_profile_steps_tbl"
    set valid_to = clock_timestamp()
    where id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
