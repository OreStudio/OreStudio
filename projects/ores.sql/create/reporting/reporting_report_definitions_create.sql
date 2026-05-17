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
 *  Table
 *
 * The persistent template for a report. Describes what to run, when to run it,
 * and how to handle concurrent executions. Type-specific configuration (e.g.
 * risk parameters) lives in a separate table keyed by report_definition_id.
 *
 * Lifecycle is managed through the report_definition_lifecycle FSM machine.
 * fsm_state_id points to the current state in ores_dq_fsm_states_tbl.
 *
 * scheduler_job_id links to ores_scheduler_job_definitions_tbl.id and is set
 * by the scheduler service when the definition is activated (state: active).
 * It is cleared when the definition is suspended or archived.
 */

create table if not exists "ores_reporting_report_definitions_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "party_id" uuid not null,
    "description" text not null default '',
    "report_type" text not null,
    "fsm_state_id" uuid null,
    "schedule_expression" text not null,
    "concurrency_policy" text not null default 'skip',
    "scheduler_job_id" uuid null,
    "workspace_id" integer not null default 0 references ores_workspaces_tbl(id),
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
    check ("name" <> ''),
    check ("schedule_expression" <> '')
);

-- Unique name for active records
create unique index if not exists report_definitions_name_uniq_idx
on "ores_reporting_report_definitions_tbl" (tenant_id, party_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists report_definitions_version_uniq_idx
on "ores_reporting_report_definitions_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists report_definitions_id_uniq_idx
on "ores_reporting_report_definitions_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists report_definitions_tenant_idx
on "ores_reporting_report_definitions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists report_definitions_scheduler_job_id_uniq_idx
on "ores_reporting_report_definitions_tbl" (scheduler_job_id)
where valid_to = ores_utility_infinity_timestamp_fn()
  and scheduler_job_id is not null;

create index if not exists report_definitions_party_idx
on "ores_reporting_report_definitions_tbl" (tenant_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists report_definitions_fsm_state_idx
on "ores_reporting_report_definitions_tbl" (tenant_id, fsm_state_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists report_definitions_workspace_idx
on "ores_reporting_report_definitions_tbl" (workspace_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_report_definitions_insert_fn()
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

    -- Validate fsm_state_id (optional soft FK to ores_dq_fsm_states_tbl)
    if NEW.fsm_state_id is not null then
        if not exists (
            select 1 from ores_dq_fsm_states_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and id = NEW.fsm_state_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid fsm_state_id: %. No active FSM state found with this id.', NEW.fsm_state_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate report_type
    NEW.report_type := ores_reporting_validate_report_type_fn(NEW.tenant_id, NEW.report_type);

    -- Validate concurrency_policy
    NEW.concurrency_policy := ores_reporting_validate_concurrency_policy_fn(NEW.tenant_id, NEW.concurrency_policy);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_reporting_report_definitions_tbl"
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

        update "ores_reporting_report_definitions_tbl"
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
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_reporting_report_definitions_insert_trg
before insert on "ores_reporting_report_definitions_tbl"
for each row execute function ores_reporting_report_definitions_insert_fn();

create or replace rule ores_reporting_report_definitions_delete_rule as
on delete to "ores_reporting_report_definitions_tbl" do instead
    update "ores_reporting_report_definitions_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
