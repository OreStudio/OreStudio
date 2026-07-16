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
 * Business Unit Type Table
 *
 * Defines the type and level of a business unit (e.g. Division, Desk).
 * The level field enforces hierarchy ordering: 0 = top-level (e.g.
 * Division or Branch), higher values = lower in the hierarchy.
 * Business units referencing a type must satisfy: child.level >
 * parent.level.
 */

create table if not exists "ores_refdata_business_unit_types_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "coding_scheme_code" text not null,
    "code" text not null,
    "name" text not null,
    "level" integer not null default 0,
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
    check ("level" >= 0)
);

-- Composite natural key: unique combination for active records
create unique index if not exists business_unit_types_coding_scheme_code_code_uniq_idx
on "ores_refdata_business_unit_types_tbl" (tenant_id, coding_scheme_code, code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists business_unit_types_version_uniq_idx
on "ores_refdata_business_unit_types_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists business_unit_types_id_uniq_idx
on "ores_refdata_business_unit_types_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists business_unit_types_tenant_idx
on "ores_refdata_business_unit_types_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_business_unit_types_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate coding_scheme_code (soft FK against dq_coding_schemes,
    -- always system-tenant-scoped: coding schemes are shared reference
    -- data, not per-tenant, unlike most soft FKs in this file).
    if not exists (
        select 1 from ores_dq_coding_schemes_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.coding_scheme_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid coding_scheme_code: %. Must be a valid coding scheme.',
            NEW.coding_scheme_code
            using errcode = '23503';
    end if;
    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_refdata_business_unit_types_tbl"
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
        update "ores_refdata_business_unit_types_tbl"
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

create or replace trigger ores_refdata_business_unit_types_insert_trg
before insert on "ores_refdata_business_unit_types_tbl"
for each row execute function ores_refdata_business_unit_types_insert_fn();

create or replace rule ores_refdata_business_unit_types_delete_rule as
on delete to "ores_refdata_business_unit_types_tbl" do instead (
    update "ores_refdata_business_unit_types_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
-- =============================================================================
-- Validation function for business_unit_type
-- Validates that a UUID id exists in the business_unit_types table.
-- Returns the id if found and active, raises exception otherwise.
-- Used by other tables' (e.g. business_units) soft FK validation, so it
-- is defined here as a trailing paste block rather than the standard
-- per-column FK/text_code_validations knobs, which only validate this
-- table's own columns, not export a reusable validator for others.
-- =============================================================================
create or replace function ores_refdata_validate_business_unit_type_fn(
    p_tenant_id uuid,
    p_id uuid
) returns uuid as $$
begin
    if p_id is null then
        raise exception 'Invalid business_unit_type id: value cannot be null'
            using errcode = '23502';
    end if;

    -- Allow pass-through during bootstrap (empty table)
    if not exists (select 1 from ores_refdata_business_unit_types_tbl
                   where tenant_id = p_tenant_id limit 1) then
        return p_id;
    end if;

    if not exists (
        select 1 from ores_refdata_business_unit_types_tbl
        where tenant_id = p_tenant_id
          and id = p_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid business_unit_type id: %. No active business unit type found.',
            p_id using errcode = '23503';
    end if;

    return p_id;
end;
$$ language plpgsql;
