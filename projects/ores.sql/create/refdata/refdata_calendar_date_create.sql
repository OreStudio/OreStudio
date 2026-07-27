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
 * Template: sql_schema_junction_create.mustache
 * To modify, update the template and regenerate.
 *
 * Calendar Date Table
 *
 * One row per (calendar_code, date), produced by the calendar
 * materialisation service instantiating a
 * [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]] template's
 * [[id:875E96F6-3FC7-4E0B-8E3C-2AC0F8BD488F][calendar_rule]] /
 * [[id:8B6E1C53-871E-4B6D-BD20-CB3F551B5C46][calendar_exception]] rows
 * over a rolling date range, never edited by hand. Read by every
 * consumer that needs "is date D a business day for calendar C" --
 * the UI (calendar detail screens, the holiday-aware date picker) and
 * ORE Studio's own tenor/schedule date-math -- so the answer is
 * consistent and never requires a live rule evaluation.
 */

create table if not exists "ores_refdata_calendar_dates_tbl" (
    "calendar_code" text not null,
    "tenant_id" uuid not null,
    "date" date not null,
    "version" integer not null,
    "is_business_day" boolean not null default false,
    "source" text not null default 'quantlib_computed',
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, calendar_code, date, valid_from),
    exclude using gist (
        tenant_id WITH =,
        calendar_code WITH =,
        date WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for looking up dates for a calendar
create index if not exists calendar_dates_calendar_idx
on "ores_refdata_calendar_dates_tbl" (calendar_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding calendars active on a date
create index if not exists calendar_dates_date_idx
on "ores_refdata_calendar_dates_tbl" (date)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists calendar_dates_uniq_idx
on "ores_refdata_calendar_dates_tbl" (tenant_id, calendar_code, date)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists calendar_dates_tenant_idx
on "ores_refdata_calendar_dates_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_calendar_dates_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Version management
    select version into current_version
    from "ores_refdata_calendar_dates_tbl"
    where tenant_id = new.tenant_id
    and calendar_code = new.calendar_code
    and date = new.date
    and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        -- Close existing record
        update "ores_refdata_calendar_dates_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
        and calendar_code = new.calendar_code
        and date = new.date
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_refdata_calendar_dates_insert_trg
before insert on "ores_refdata_calendar_dates_tbl"
for each row
execute function ores_refdata_calendar_dates_insert_fn();

create or replace rule ores_refdata_calendar_dates_delete_rule as
on delete to "ores_refdata_calendar_dates_tbl"
do instead
  update "ores_refdata_calendar_dates_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and calendar_code = old.calendar_code
  and date = old.date
  and valid_to = ores_utility_infinity_timestamp_fn();
