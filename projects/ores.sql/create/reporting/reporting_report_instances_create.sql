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
 * Report Instances Table
 *
 * A single execution of a report_definition. Created automatically when the
 * scheduler fires a trigger for an active definition.
 *
 * Lifecycle is managed through the report_instance_lifecycle FSM machine.
 * fsm_state_id points to the current state in ores_dq_fsm_states_tbl.
 *
 * The initial fsm_state depends on the definition's concurrency_policy and
 * whether another instance for the same definition is already running:
 *   - No running instance          -> pending
 *   - Running + policy = queue     -> queued
 *   - Running + policy = skip      -> skipped (terminal)
 *   - Running + policy = fail      -> failed  (terminal)
 *
 * started_at is NULL when the instance is cancelled or skipped before execution
 * begins. completed_at is NULL while running or in a terminal-before-start state.
 */

create table if not exists "ores_reporting_report_instances_tbl" (
    "id"               uuid    not null,
    "tenant_id"        uuid    not null,
    "version"          integer not null,
    "name"             text    not null,
    "description"      text    not null default '',
    "party_id"         uuid    not null,
    "definition_id"    uuid    not null,
    "fsm_state_id"     uuid    null,
    "trigger_run_id"   bigint  not null,
    "output_message"   text    not null default '',
    "started_at"       timestamp with time zone null,
    "completed_at"     timestamp with time zone null,
    "modified_by"      text    not null,
    "performed_by"     text    not null,
    "change_reason_code" text  not null,
    "change_commentary"  text  not null,
    "valid_from"       timestamp with time zone not null,
    "valid_to"         timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("name" <> ''),
    check ("completed_at" is null or "started_at" is not null)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_reporting_report_instances_version_uniq_idx
on "ores_reporting_report_instances_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Current record uniqueness
create unique index if not exists ores_reporting_report_instances_id_uniq_idx
on "ores_reporting_report_instances_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Natural key: unique instance name per tenant
create unique index if not exists ores_reporting_report_instances_name_uniq_idx
on "ores_reporting_report_instances_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Tenant index
create index if not exists ores_reporting_report_instances_tenant_idx
on "ores_reporting_report_instances_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Definition index for listing instances per definition
create index if not exists ores_reporting_report_instances_definition_idx
on "ores_reporting_report_instances_tbl" (tenant_id, definition_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- FSM state index for state-based filtering (e.g. "find all running instances")
create index if not exists ores_reporting_report_instances_fsm_state_idx
on "ores_reporting_report_instances_tbl" (tenant_id, fsm_state_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_report_instances_insert_fn()
returns trigger as $$
declare
    current_version integer;
    def_name        text;
    def_description text;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Validate party_id (soft FK to ores_refdata_parties_tbl)
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = new.tenant_id
          and id = new.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_id: %. No active party found with this id.',
            new.party_id
            using errcode = '23503';
    end if;

    -- Validate definition_id; copy name/description from definition if not provided
    select name, description into def_name, def_description
    from ores_reporting_report_definitions_tbl
    where tenant_id = new.tenant_id
      and id = new.definition_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'Invalid definition_id: %. No active report definition found with this id.',
            new.definition_id
            using errcode = '23503';
    end if;

    if new.name = '' then
        new.name := def_name;
    end if;
    if new.description = '' then
        new.description := coalesce(def_description, '');
    end if;

    -- Validate fsm_state_id (soft FK to ores_dq_fsm_states_tbl)
    if new.fsm_state_id is not null then
        if not exists (
            select 1 from ores_dq_fsm_states_tbl
            where tenant_id = ores_iam_system_tenant_id_fn()
              and id = new.fsm_state_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid fsm_state_id: %. No active FSM state found with this id.',
                new.fsm_state_id
                using errcode = '23503';
        end if;
    end if;

    -- Version management
    select version into current_version
    from "ores_reporting_report_instances_tbl"
    where tenant_id = new.tenant_id
      and id = new.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "ores_reporting_report_instances_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
          and id = new.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_actor_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql security definer set search_path = public;

create or replace trigger ores_reporting_report_instances_insert_trg
before insert on "ores_reporting_report_instances_tbl"
for each row execute function ores_reporting_report_instances_insert_fn();

create or replace rule ores_reporting_report_instances_delete_rule as
on delete to "ores_reporting_report_instances_tbl" do instead
    update "ores_reporting_report_instances_tbl"
    set valid_to = current_timestamp
    where tenant_id = old.tenant_id
      and id = old.id
      and valid_to = ores_utility_infinity_timestamp_fn();
