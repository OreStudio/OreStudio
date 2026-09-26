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
 * Workflow Step Table
 *
 * Records the execution of one step in a saga workflow, including the step
 * index, name, status, request/response payloads, and timing. Steps reference
 * their parent workflow instance via workflow_id.
 */

create table if not exists "ores_workflow_workflow_steps_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "workflow_id" uuid not null,
    "step_index" integer not null,
    "name" text not null,
    "state_id" uuid not null,
    "request_json" jsonb not null,
    "response_json" jsonb null,
    "error" text null,
    "step_log_json" jsonb null,
    "command_subject" text not null default '',
    "command_json" text not null default '',
    "command_published_at" timestamp with time zone null,
    "idempotency_key" text not null default '',
    "compensation_subject" text not null default '',
    "compensation_json" text not null default '',
    "started_at" timestamp with time zone null,
    "completed_at" timestamp with time zone null,
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

-- Version uniqueness for optimistic concurrency
create unique index if not exists workflow_steps_version_uniq_idx
on "ores_workflow_workflow_steps_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists workflow_steps_id_uniq_idx
on "ores_workflow_workflow_steps_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists workflow_steps_tenant_idx
on "ores_workflow_workflow_steps_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists workflow_steps_workflow_id_idx
on "ores_workflow_workflow_steps_tbl" (workflow_id);

create or replace function ores_workflow_workflow_steps_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate workflow_id (soft FK to ores_workflow_workflow_instances_tbl)
    if not exists (
        select 1 from ores_workflow_workflow_instances_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.workflow_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid workflow_id: %. No active workflow instance found with this id.', NEW.workflow_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_workflow_workflow_steps_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
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
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_workflow_workflow_steps_tbl"
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

create or replace trigger ores_workflow_workflow_steps_insert_trg
before insert on "ores_workflow_workflow_steps_tbl"
for each row execute function ores_workflow_workflow_steps_insert_fn();

create or replace rule ores_workflow_workflow_steps_delete_rule as
on delete to "ores_workflow_workflow_steps_tbl" do instead (
    update "ores_workflow_workflow_steps_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
