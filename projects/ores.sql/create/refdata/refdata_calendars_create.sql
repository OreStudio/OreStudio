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
 * Calendar Table
 *
 * Validated enumeration of named date collections consumed by ORE and
 * QuantLib: business-day/holiday calendars (TARGET, UnitedStates,
 * UnitedStates.GovernmentBond, ...), central-bank meeting calendars,
 * and other calendar-shaped reference data. Each row is one concrete
 * QuantLib/ORE calendar token — sub-market variants (e.g.
 * UnitedStates.NYSE vs UnitedStates.GovernmentBond) are separate
 * rows, not a joined variant field, so the code column always matches
 * ORE's XML <Calendar> vocabulary verbatim. Classified by
 * [[id:1A454661-81B5-4F8F-93A6-06547412DD84][calendar_type]] and
 * associated with the [[id:88E8E1FB-6F2F-495F-BEC4-8C7ABEF68563][country]]
 * whose calendar it is — supranational calendars (TARGET) use the
 * ZZ sentinel (ISO 3166-1's own reserved user-assigned code) rather
 * than a nullable country reference, since no single country owns them.
 */

create table if not exists "ores_refdata_calendars_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "calendar_type" text not null default 'public_holiday',
    "country_code" text not null default 'ZZ',
    "image_id" uuid null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, code, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("code" <> '')
);

-- Unique name for active records
create unique index if not exists calendars_name_uniq_idx
on "ores_refdata_calendars_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists calendars_version_uniq_idx
on "ores_refdata_calendars_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists calendars_code_uniq_idx
on "ores_refdata_calendars_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists calendars_tenant_idx
on "ores_refdata_calendars_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_calendars_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate calendar_type
    NEW.calendar_type := ores_refdata_validate_calendar_type_fn(NEW.tenant_id, NEW.calendar_type);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Paste block: doesn't fit soft_fk_validations (joins on id, not
    -- an alpha-2 code column) or the Validations table
    -- (ores_refdata_validate_country_fn only checks non-null/empty,
    -- not existence).
    if not exists (
        select 1 from ores_refdata_countries_tbl
        where tenant_id = NEW.tenant_id
          and alpha2_code = NEW.country_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid country_code: %. Must be a valid ISO 3166-1 alpha-2 code (or the ZZ sentinel).',
            NEW.country_code;
    end if;
    -- Version management
    select version into current_version
    from "ores_refdata_calendars_tbl"
    where tenant_id = NEW.tenant_id
      and code = NEW.code
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
        update "ores_refdata_calendars_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and code = NEW.code
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

create or replace trigger ores_refdata_calendars_insert_trg
before insert on "ores_refdata_calendars_tbl"
for each row execute function ores_refdata_calendars_insert_fn();

create or replace rule ores_refdata_calendars_delete_rule as
on delete to "ores_refdata_calendars_tbl" do instead (
    update "ores_refdata_calendars_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Validation function for calendar
-- Validates that a code exists in the calendars table.
-- Returns the validated value, or default if null/empty.
-- Validates against both the tenant's own data and the system tenant's canonical set.
-- =============================================================================
create or replace function ores_refdata_validate_calendar_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- Return default if null or empty
    if p_value is null or p_value = '' then
        raise exception 'Invalid calendar: value cannot be null or empty'
            using errcode = '23502';
    end if;

    -- Allow pass-through if neither this tenant nor the system tenant has
    -- seeded active calendars yet (freshly provisioned tenant).
    if not exists (
        select 1 from ores_refdata_calendars_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    -- Validate against this tenant's values and the system tenant's canonical set.
    if not exists (
        select 1 from ores_refdata_calendars_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and code = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid calendar: %. Must be one of: %', p_value, (
            select string_agg(code::text, ', ' order by code)
            from ores_refdata_calendars_tbl
            where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
