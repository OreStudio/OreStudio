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
 * FSM Transitions Table
 *
 * Defines valid state-to-state transitions within a machine. Both from_state_id
 * and to_state_id must belong to the same machine. Optionally references a
 * guard function for business rule enforcement.
 */

create table if not exists "ores_fsm_transitions_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "machine_id" uuid not null,
    "from_state_id" uuid not null,
    "to_state_id" uuid not null,
    "name" text not null,
    "guard_function" text null,
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
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_fsm_transitions_version_uniq_idx
on "ores_fsm_transitions_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Current record uniqueness
create unique index if not exists ores_fsm_transitions_id_uniq_idx
on "ores_fsm_transitions_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Prevent duplicate transitions per machine (same from/to state pair)
create unique index if not exists ores_fsm_transitions_machine_states_uniq_idx
on "ores_fsm_transitions_tbl" (tenant_id, machine_id, from_state_id, to_state_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Tenant index for efficient filtering
create index if not exists ores_fsm_transitions_tenant_idx
on "ores_fsm_transitions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Machine index for efficient transition lookups
create index if not exists ores_fsm_transitions_machine_idx
on "ores_fsm_transitions_tbl" (tenant_id, machine_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_fsm_transitions_insert_fn()
returns trigger as $$
declare
    current_version integer;
    from_machine_id uuid;
    to_machine_id uuid;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate machine_id (mandatory soft FK)
    if not exists (
        select 1 from ores_fsm_machines_tbl
        where tenant_id = NEW.tenant_id and id = NEW.machine_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid machine_id: %. No active FSM machine found with this id.',
            NEW.machine_id
            using errcode = '23503';
    end if;

    -- Validate from_state_id (mandatory soft FK) and retrieve its machine
    select machine_id into from_machine_id
    from ores_fsm_states_tbl
    where tenant_id = NEW.tenant_id and id = NEW.from_state_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'Invalid from_state_id: %. No active FSM state found with this id.',
            NEW.from_state_id
            using errcode = '23503';
    end if;

    -- Validate to_state_id (mandatory soft FK) and retrieve its machine
    select machine_id into to_machine_id
    from ores_fsm_states_tbl
    where tenant_id = NEW.tenant_id and id = NEW.to_state_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'Invalid to_state_id: %. No active FSM state found with this id.',
            NEW.to_state_id
            using errcode = '23503';
    end if;

    -- Ensure both states belong to the declared machine
    if from_machine_id <> NEW.machine_id then
        raise exception 'from_state_id % belongs to machine %, not the declared machine_id %.',
            NEW.from_state_id, from_machine_id, NEW.machine_id
            using errcode = '23503';
    end if;

    if to_machine_id <> NEW.machine_id then
        raise exception 'to_state_id % belongs to machine %, not the declared machine_id %.',
            NEW.to_state_id, to_machine_id, NEW.machine_id
            using errcode = '23503';
    end if;

    -- Version management
    select version into current_version
    from "ores_fsm_transitions_tbl"
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

        update "ores_fsm_transitions_tbl"
        set valid_to = current_timestamp
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();

    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = current_user;

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_fsm_transitions_insert_trg
before insert on "ores_fsm_transitions_tbl"
for each row execute function ores_fsm_transitions_insert_fn();

create or replace rule ores_fsm_transitions_delete_rule as
on delete to "ores_fsm_transitions_tbl" do instead
    update "ores_fsm_transitions_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
