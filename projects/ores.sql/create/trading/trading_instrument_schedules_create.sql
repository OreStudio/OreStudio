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
 * Instrument Schedule Table
 *
 * One row per schedule an instrument's leg states, keyed to the
 * instrument, the leg that states it and the schedule's role.
 *
 * The ORE schema states a schedule as a choice: a rule block that a
 * calendar expands, or an explicit list of dates. The generated code
 * mirrors the choice as two lists on bond_schedule_data, and the
 * mapping container carries both. This table holds the rule arm's
 * scalars and the schedule_kind column records which arm the document
 * chose, so a reader rebuilds the arm it read rather than guessing. The
 * date arm's rows land in the keyed child table.
 *
 * A bond leg states four schedules: its own schedule, its payment
 * schedule, and, when the leg is floating, the fixing and reset
 * schedules. The role column names which one, so one table holds all
 * four.
 *
 * The nine bond tables carry no schedule column, and the org models of
 * the family record this table as the destination. It is keyed by the
 * instrument rather than by a bond row, so any family whose product
 * states a schedule writes here without a new table.
 *
 * Every scalar the schema declares optional is nullable here and an
 * std::optional in C++, so a member the document states and the row
 * cannot hold stays distinguishable from one the document omits.
 */

create table if not exists "ores_trading_instrument_schedules_tbl" (
    "instrument_id" uuid not null,
    "leg_role" text not null,
    "leg_number" integer not null,
    "schedule_role" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "schedule_kind" text not null,
    "start_date" text null,
    "end_date" text null,
    "adjust_end_date_to_previous_month_end" text null,
    "tenor" text null,
    "calendar" text null,
    "convention" text null,
    "term_convention" text null,
    "rule" text null,
    "end_of_month" text null,
    "end_of_month_convention" text null,
    "first_date" text null,
    "last_date" text null,
    "remove_first_date" boolean null,
    "remove_last_date" boolean null,
    "include_duplicate_dates" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, instrument_id, leg_role, leg_number, schedule_role, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        instrument_id WITH =,
        leg_role WITH =,
        leg_number WITH =,
        schedule_role WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("instrument_id" <> ores_utility_nil_uuid_fn()),
    check ("leg_role" <> ''),
    check ("schedule_role" <> ''),
    check ("schedule_kind" in ('rules', 'dates')),
    check ("leg_role" in ('bond', 'trs_funding', 'repo', 'ascot_swap')),
    check ("leg_number" > 0)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists instrument_schedules_version_uniq_idx
on "ores_trading_instrument_schedules_tbl" (tenant_id, instrument_id, leg_role, leg_number, schedule_role, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists instrument_schedules_id_uniq_idx
on "ores_trading_instrument_schedules_tbl" (tenant_id, instrument_id, leg_role, leg_number, schedule_role)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists instrument_schedules_tenant_idx
on "ores_trading_instrument_schedules_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_instrument_schedules_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_instrument_schedules_tbl"
    where tenant_id = NEW.tenant_id
      and instrument_id = NEW.instrument_id and leg_role = NEW.leg_role and leg_number = NEW.leg_number and schedule_role = NEW.schedule_role
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
        update "ores_trading_instrument_schedules_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and instrument_id = NEW.instrument_id and leg_role = NEW.leg_role and leg_number = NEW.leg_number and schedule_role = NEW.schedule_role
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

create or replace trigger ores_trading_instrument_schedules_insert_trg
before insert on "ores_trading_instrument_schedules_tbl"
for each row execute function ores_trading_instrument_schedules_insert_fn();

create or replace rule ores_trading_instrument_schedules_delete_rule as
on delete to "ores_trading_instrument_schedules_tbl" do instead (
    update "ores_trading_instrument_schedules_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and instrument_id = OLD.instrument_id and leg_role = OLD.leg_role and leg_number = OLD.leg_number and schedule_role = OLD.schedule_role
      and valid_to = ores_utility_infinity_timestamp_fn();
);
