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
 * Trade State Table
 *
 * A trade's lifecycle state: where it stands in the trade_status machine
 * (draft, live, expired or cancelled). It is a component of the [[id:4304A441-E532-45FB-837A-378F13693CAE][trade
 * anchor]], keyed by the trade id, and versions on its own timeline: a
 * lifecycle event cuts a new version of the state and leaves the booking
 * alone.
 *
 * The status is not the caller's to supply. The activity names the event,
 * reference data maps the event to a transition, and the insert trigger
 * takes the transition only from the state it starts from. An activity that
 * maps to no transition versions the state and leaves the status where it
 * was.
 *
 * The state copies the anchor's party, pinned to the anchor, because
 * row-level security needs the party on every row.
 */

create table if not exists "ores_trading_trade_states_tbl" (
    "trade_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "activity_type_code" text not null,
    "status_id" uuid not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, trade_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        trade_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("trade_id" <> ores_utility_nil_uuid_fn()),
    constraint ores_trading_trade_states_trade_id_fk foreign key ("tenant_id", "trade_id") references "ores_trading_trades_tbl" ("tenant_id", "id"),
    constraint ores_trading_trade_states_anchor_party_pin foreign key ("tenant_id", "trade_id", "party_id") references "ores_trading_trades_tbl" ("tenant_id", "id", "party_id")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists trade_states_version_uniq_idx
on "ores_trading_trade_states_tbl" (tenant_id, trade_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists trade_states_id_uniq_idx
on "ores_trading_trade_states_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_states_tenant_idx
on "ores_trading_trade_states_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_states_status_idx
on "ores_trading_trade_states_tbl" (tenant_id, status_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_trade_states_insert_fn()
returns trigger as $$
declare
    current_version integer;
    v_transition record;
    v_prior_status_id uuid;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate activity_type_code (soft FK to ores_trading_activity_types_tbl)
    if not exists (
        select 1 from ores_trading_activity_types_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.activity_type_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid activity_type_code: %. No active activity type found with this code.', NEW.activity_type_code
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
    from "ores_trading_trade_states_tbl"
    where tenant_id = NEW.tenant_id
      and trade_id = NEW.trade_id
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
        select status_id into v_prior_status_id
        from "ores_trading_trade_states_tbl"
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        v_transition := ores_trading_resolve_trade_transition_fn(NEW.activity_type_code);

        if not v_transition.has_transition then
            NEW.status_id = v_prior_status_id;
        else
            if v_transition.from_state_id is null then
                raise exception 'Activity % can only book a trade: transition % starts the machine, but this trade is already at %.',
                    NEW.activity_type_code, v_transition.transition_name, v_prior_status_id
                    using errcode = '23514';
            end if;

            if v_prior_status_id is distinct from v_transition.from_state_id then
                raise exception 'Activity % is not legal here: transition % must be taken from %, but the trade is at %.',
                    NEW.activity_type_code, v_transition.transition_name,
                    v_transition.from_state_id, v_prior_status_id
                    using errcode = '23514';
            end if;

            NEW.status_id = v_transition.to_state_id;
        end if;

        if ores_trading_trade_booked_virtual_fn(NEW.tenant_id, NEW.trade_id)
           and not ores_trading_status_may_be_virtual_fn(NEW.tenant_id, NEW.trade_id, NEW.status_id) then
            raise exception 'Invalid activity_type_code: %. An actual trade in a virtual book can only be a draft; book it into a real book first.',
                NEW.activity_type_code
                using errcode = '23514';
        end if;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_trade_states_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    v_transition := ores_trading_resolve_trade_transition_fn(NEW.activity_type_code);

    if not v_transition.has_transition then
        raise exception 'Activity % cannot book a trade: it names no transition to start the machine.',
            NEW.activity_type_code
            using errcode = '23514';
    end if;
    if v_transition.from_state_id is not null then
        raise exception 'Activity % cannot book a trade: transition % leaves state %, but a new trade has no state to leave.',
            NEW.activity_type_code, v_transition.transition_name, v_transition.from_state_id
            using errcode = '23514';
    end if;
    NEW.status_id = v_transition.to_state_id;

    if ores_trading_trade_booked_virtual_fn(NEW.tenant_id, NEW.trade_id)
       and not ores_trading_status_may_be_virtual_fn(NEW.tenant_id, NEW.trade_id, NEW.status_id) then
        raise exception 'Invalid activity_type_code: %. An actual trade in a virtual book can only be a draft.',
            NEW.activity_type_code
            using errcode = '23514';
    end if;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_trade_states_insert_trg
before insert on "ores_trading_trade_states_tbl"
for each row execute function ores_trading_trade_states_insert_fn();

create or replace rule ores_trading_trade_states_delete_rule as
on delete to "ores_trading_trade_states_tbl" do instead (
    update "ores_trading_trade_states_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
