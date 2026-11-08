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
 * Scripted Parameter Table
 *
 * One named parameter of a scripted trade, keyed to the trade and the
 * parameter's ordinal.
 *
 * ORE reads every scripted trade as a script and a set of named parameters of
 * five kinds: event, number, index, currency and day counter
 * (scriptedtrade.cpp). A generic trade states them under
 * ScriptedTradeData/Data as <Number><Name/><Value/></Number>; a named
 * product such as MovingMaxYYIISData states each as an element named after
 * the variable, typed by its type attribute. Either way a parameter is a
 * name, a type and a value, and the value is a scalar, a list, or for an
 * event a schedule or a schedule derived from another event. The list is
 * finite, so every scripted product is these rows.
 *
 * A list value is scripted_parameter_value rows, and an event's schedule is
 * rows of the shared schedule tables under the owner scripted and the
 * parameter's ordinal.
 */

create table if not exists "ores_trading_scripted_parameters_tbl" (
    "trade_id" uuid not null,
    "sequence_number" integer not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_activity_id" uuid not null,
    "name" text not null,
    "declared_type" text not null,
    "shape" text not null,
    "value" text null,
    "apply_coarsening" text null,
    "base_schedule" text null,
    "shift" text null,
    "calendar" text null,
    "convention" text null,
    "remove_first_date" text null,
    "remove_last_date" text null,
    "shift_unit" text null,
    "shift_anchor" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, trade_id, sequence_number, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        trade_id WITH =,
        sequence_number WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("trade_id" <> ores_utility_nil_uuid_fn()),
    check ("sequence_number" >= 1),
    check ("shape" in ('scalar', 'vector', 'schedule', 'derived_schedule'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists scripted_parameters_version_uniq_idx
on "ores_trading_scripted_parameters_tbl" (tenant_id, trade_id, sequence_number, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists scripted_parameters_id_uniq_idx
on "ores_trading_scripted_parameters_tbl" (tenant_id, trade_id, sequence_number)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists scripted_parameters_tenant_idx
on "ores_trading_scripted_parameters_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists scripted_parameters_trade_id_idx
on "ores_trading_scripted_parameters_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_scripted_parameters_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate trade_id (soft FK to ores_trading_trades_tbl)
    if not exists (
        select 1 from ores_trading_trades_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.trade_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid trade_id: %. Trade must exist for tenant.', NEW.trade_id
            using errcode = '23503';
    end if;

    -- Validate trade_activity_id (soft FK to ores_trading_trade_activities_tbl)
    if not exists (
        select 1 from ores_trading_trade_activities_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.trade_activity_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid trade_activity_id: %. No active trade activity found with this id.', NEW.trade_activity_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_trading_scripted_parameters_tbl"
    where tenant_id = NEW.tenant_id
      and trade_id = NEW.trade_id and sequence_number = NEW.sequence_number
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
                    'scripted_parameter',
                    'trade_id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'scripted_parameter',
                'trade_id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_scripted_parameters_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id and sequence_number = NEW.sequence_number
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

create or replace trigger ores_trading_scripted_parameters_insert_trg
before insert on "ores_trading_scripted_parameters_tbl"
for each row execute function ores_trading_scripted_parameters_insert_fn();

create or replace rule ores_trading_scripted_parameters_delete_rule as
on delete to "ores_trading_scripted_parameters_tbl" do instead (
    update "ores_trading_scripted_parameters_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id and sequence_number = OLD.sequence_number
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Scripted Parameter
-- =============================================================================
alter table ores_trading_scripted_parameters_tbl enable row level security;

drop policy if exists scripted_parameters_tbl_tenant_isolation_policy
    on ores_trading_scripted_parameters_tbl;

create policy scripted_parameters_tbl_tenant_isolation_policy
on ores_trading_scripted_parameters_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
