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
 * Tenor Schedule Table
 *
 * A named schedule axis a [[id:9A2E4D6B-7C1F-4B8A-A5D3-2F6E9B1C4A87][tenor]]
 * resolves along (story Decision D2: anchor + calendar offset + n
 * steps). Two kinds today, distinguished by schedule_source:
 *
 * - CLOSED_FORM: the dates come from a closed-form rule evaluated
 *   code-side. ROLL_QUARTER is the only instance: the first business
 *   day after the 20th of March/June/September/December (the IMM
 *   quarterly rule).
 * - EVENT_LOOKUP: the dates come from
 *   [[id:B20050A5-1245-4944-A328-2A0893C92AEC][calendar_event]] rows on a
 *   named calendar, filtered by diary entry type. FOMC_MEETING is the
 *   only instance: central_bank_meeting events on US.FOMC.
 *
 * calendar_code and diary_entry_type are null for closed-form
 * schedules (no event store involved) and required for event-lookup
 * ones -- but the binding is documented, not enforced in the schema.
 */

create table if not exists "ores_refdata_tenor_schedules_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "display_order" integer not null default 0,
    "schedule_source" text not null default 'CLOSED_FORM',
    "calendar_code" text null,
    "diary_entry_type" text null,
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
create unique index if not exists tenor_schedules_name_uniq_idx
on "ores_refdata_tenor_schedules_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists tenor_schedules_version_uniq_idx
on "ores_refdata_tenor_schedules_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists tenor_schedules_code_uniq_idx
on "ores_refdata_tenor_schedules_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists tenor_schedules_tenant_idx
on "ores_refdata_tenor_schedules_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_tenor_schedules_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate calendar_code (optional soft FK to ores_refdata_calendars_tbl)
    if NEW.calendar_code is not null then
        if not exists (
            select 1 from ores_refdata_calendars_tbl
            where tenant_id = NEW.tenant_id
              and code = NEW.calendar_code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid calendar_code: %. No active calendar found with this code.', NEW.calendar_code
                using errcode = '23503';
        end if;
    end if;

    -- Validate diary_entry_type (optional soft FK to ores_refdata_diary_entry_types_tbl)
    if NEW.diary_entry_type is not null then
        if not exists (
            select 1 from ores_refdata_diary_entry_types_tbl
            where tenant_id = NEW.tenant_id
              and code = NEW.diary_entry_type
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid diary_entry_type: %. No active diary entry type found with this code.', NEW.diary_entry_type
                using errcode = '23503';
        end if;
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    if NEW.schedule_source not in ('CLOSED_FORM', 'EVENT_LOOKUP') then
        raise exception 'Invalid schedule_source: %. Must be CLOSED_FORM or EVENT_LOOKUP.',
            NEW.schedule_source;
    end if;
    -- Version management
    select version into current_version
    from "ores_refdata_tenor_schedules_tbl"
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
        update "ores_refdata_tenor_schedules_tbl"
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

create or replace trigger ores_refdata_tenor_schedules_insert_trg
before insert on "ores_refdata_tenor_schedules_tbl"
for each row execute function ores_refdata_tenor_schedules_insert_fn();

create or replace rule ores_refdata_tenor_schedules_delete_rule as
on delete to "ores_refdata_tenor_schedules_tbl" do instead (
    update "ores_refdata_tenor_schedules_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Validation function for tenor_schedule
-- Validates that a code exists in the tenor_schedules table.
-- Returns the validated value, or default if null/empty.
-- Uses system tenant data (shared reference data).
-- =============================================================================
create or replace function ores_refdata_validate_tenor_schedule_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- Return default if null or empty
    if p_value is null or p_value = '' then
        raise exception 'Invalid tenor_schedule: value cannot be null or empty'
            using errcode = '23502';
    end if;

    -- Allow pass-through during bootstrap (no active rows for system tenant).
    if not exists (
        select 1 from ores_refdata_tenor_schedules_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    -- Validate against reference data
    if not exists (
        select 1 from ores_refdata_tenor_schedules_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid tenor_schedule: %. Must be one of: %', p_value, (
            select string_agg(code::text, ', ' order by display_order)
            from ores_refdata_tenor_schedules_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
