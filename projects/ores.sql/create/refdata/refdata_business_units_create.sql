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
 * Business Unit Table
 *
 * Represents internal organizational units (e.g., desks, departments, branches).
 * Supports hierarchical structure via self-referencing parent_business_unit_id.
 * Each unit belongs to a top-level legal entity (party).
 */

create table if not exists "ores_refdata_business_units_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "unit_name" text not null,
    "parent_business_unit_id" uuid null,
    "unit_code" text null,
    "business_centre_code" text null,
    "unit_type_id" uuid null,
    "status" text not null default 'Active',
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
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Composite natural key: unique combination for active records
create unique index if not exists business_units_party_id_unit_name_uniq_idx
on "ores_refdata_business_units_tbl" (tenant_id, party_id, unit_name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists business_units_version_uniq_idx
on "ores_refdata_business_units_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists business_units_id_uniq_idx
on "ores_refdata_business_units_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists business_units_tenant_idx
on "ores_refdata_business_units_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_business_units_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate party_id (soft FK to ores_refdata_parties_tbl)
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_id: %. No active party found with this id.', NEW.party_id
            using errcode = '23503';
    end if;

    -- Validate parent_business_unit_id (optional soft FK to ores_refdata_business_units_tbl)
    if NEW.parent_business_unit_id is not null then
        if not exists (
            select 1 from ores_refdata_business_units_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.parent_business_unit_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid parent_business_unit_id: %. No active business unit found with this id.', NEW.parent_business_unit_id
                using errcode = '23503';
        end if;

        -- Reject a parent_business_unit_id pointing back at NEW's own row, directly or
        -- transitively, since the touch-version mechanism re-fires this
        -- same trigger walking up parent_business_unit_id -- an undetected cycle would
        -- recurse without bound instead of failing cleanly.
        if exists (
            with recursive ancestor_chain as (
                select id, parent_business_unit_id as parent_id
                from ores_refdata_business_units_tbl
                where id = NEW.parent_business_unit_id
                  and valid_to = ores_utility_infinity_timestamp_fn()
                union all
                select t.id, t.parent_business_unit_id as parent_id
                from ores_refdata_business_units_tbl t
                join ancestor_chain a on t.id = a.parent_id
                where t.valid_to = ores_utility_infinity_timestamp_fn()
            )
            select 1 from ancestor_chain where id = NEW.id
        ) then
            raise exception 'Invalid parent_business_unit_id: % would create a cycle in the ores_refdata_business_units_tbl hierarchy.', NEW.parent_business_unit_id
                using errcode = '23514';
        end if;
    end if;

    -- Validate business_centre_code (optional field -- skip validation when null)
    if NEW.business_centre_code is not null then
        NEW.business_centre_code := ores_refdata_validate_business_centre_fn(NEW.tenant_id, NEW.business_centre_code);
    end if;

    -- Validate unit_type_id (optional field -- skip validation when null)
    if NEW.unit_type_id is not null then
        NEW.unit_type_id := ores_refdata_validate_business_unit_type_fn(NEW.tenant_id, NEW.unit_type_id);
    end if;

    -- Level constraint: when both parent and child have a type, child level > parent level
    if NEW.unit_type_id is not null and NEW.parent_business_unit_id is not null then
        declare
            v_parent_type_id uuid;
            v_parent_level integer;
            v_child_level integer;
        begin
            select unit_type_id into v_parent_type_id
            from ores_refdata_business_units_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.parent_business_unit_id
              and valid_to = ores_utility_infinity_timestamp_fn();

            if v_parent_type_id is not null then
                select level into v_parent_level
                from ores_refdata_business_unit_types_tbl
                where tenant_id = NEW.tenant_id
                  and id = v_parent_type_id
                  and valid_to = ores_utility_infinity_timestamp_fn();

                select level into v_child_level
                from ores_refdata_business_unit_types_tbl
                where tenant_id = NEW.tenant_id
                  and id = NEW.unit_type_id
                  and valid_to = ores_utility_infinity_timestamp_fn();

                if v_child_level <= v_parent_level then
                    raise exception
                        'Business unit type level % cannot be contained by a unit of the same or higher level %. Parent level must be strictly less than child level.',
                        v_child_level, v_parent_level
                        using errcode = '23514';
                end if;
            end if;
        end;
    end if;
    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_refdata_business_units_tbl"
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
        update "ores_refdata_business_units_tbl"
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

create or replace trigger ores_refdata_business_units_insert_trg
before insert on "ores_refdata_business_units_tbl"
for each row execute function ores_refdata_business_units_insert_fn();

create or replace rule ores_refdata_business_units_delete_rule as
on delete to "ores_refdata_business_units_tbl" do instead (
    update "ores_refdata_business_units_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
