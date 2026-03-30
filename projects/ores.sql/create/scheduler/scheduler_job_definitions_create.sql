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
 * Scheduler Job Definitions Table
 *
 * Stores job definitions for OreStudio's built-in scheduler. Each row
 * represents a versioned, auditable job definition with tenant/party
 * isolation, human-readable description, and a soft-pause mechanism
 * (is_active).
 *
 * Jobs can be system-scoped (tenant_id IS NULL, party_id IS NULL),
 * tenant-scoped (tenant_id set, party_id IS NULL), or party-scoped
 * (both tenant_id and party_id set).
 *
 * The action_type column controls how the scheduler executes the job:
 *   - 'execute_sql'    : run the SQL in the command column directly.
 *   - 'nats_publish'  : publish a NATS message; action_payload carries
 *                        the subject and body.
 */

create table if not exists "ores_scheduler_job_definitions_tbl" (
    "id"                  uuid not null,
    "tenant_id"           uuid,
    "version"             integer not null,
    "party_id"            uuid,
    "job_name"            text not null,
    "description"         text not null default '',
    "command"             text not null default '',
    "schedule_expression" text not null,
    "action_type"         text not null default 'execute_sql'
                          check (action_type in ('execute_sql','nats_publish')),
    "action_payload"      jsonb not null default '{}'::jsonb,
    "is_active"           integer not null default 1,
    "modified_by"         text not null,
    "performed_by"        text not null,
    "change_reason_code"  text not null,
    "change_commentary"   text not null,
    "valid_from"          timestamp with time zone not null,
    "valid_to"            timestamp with time zone not null,
    primary key (id, valid_from, valid_to),
    exclude using gist (
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("job_name" <> ''),
    check (action_type != 'execute_sql' or "command" <> ''),
    check ("schedule_expression" <> '')
);

-- Unique job_name per tenant among active records.
-- System jobs (tenant_id IS NULL) are handled by a separate partial index.
create unique index if not exists ores_scheduler_job_definitions_name_tenant_uniq_idx
on "ores_scheduler_job_definitions_tbl" (tenant_id, job_name)
where valid_to = ores_utility_infinity_timestamp_fn() and tenant_id is not null;

create unique index if not exists ores_scheduler_job_definitions_name_system_uniq_idx
on "ores_scheduler_job_definitions_tbl" (job_name)
where valid_to = ores_utility_infinity_timestamp_fn() and tenant_id is null;

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_scheduler_job_definitions_version_uniq_idx
on "ores_scheduler_job_definitions_tbl" (id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Current record uniqueness
create unique index if not exists ores_scheduler_job_definitions_id_uniq_idx
on "ores_scheduler_job_definitions_tbl" (id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Tenant index
create index if not exists ores_scheduler_job_definitions_tenant_idx
on "ores_scheduler_job_definitions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Party index for party-scoped lookups
create index if not exists ores_scheduler_job_definitions_party_idx
on "ores_scheduler_job_definitions_tbl" (tenant_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_scheduler_job_definitions_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id only when set (system jobs have NULL tenant_id)
    if NEW.tenant_id is not null then
        NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);
    end if;

    -- Validate party_id (soft FK to ores_refdata_parties_tbl) only when set
    if NEW.party_id is not null then
        if not exists (
            select 1 from ores_refdata_parties_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.party_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid party_id: %. No active party found with this id.',
                NEW.party_id
                using errcode = '23503';
        end if;
    end if;

    -- Version management
    select version into current_version
    from "ores_scheduler_job_definitions_tbl"
    where id = NEW.id
      and (tenant_id = NEW.tenant_id or (tenant_id is null and NEW.tenant_id is null))
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update "ores_scheduler_job_definitions_tbl"
        set valid_to = current_timestamp
        where id = NEW.id
          and (tenant_id = NEW.tenant_id or (tenant_id is null and NEW.tenant_id is null))
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();

    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql security definer;

create or replace trigger ores_scheduler_job_definitions_insert_trg
before insert on "ores_scheduler_job_definitions_tbl"
for each row execute function ores_scheduler_job_definitions_insert_fn();

create or replace rule ores_scheduler_job_definitions_delete_rule as
on delete to "ores_scheduler_job_definitions_tbl" do instead
    update "ores_scheduler_job_definitions_tbl"
    set valid_to = current_timestamp
    where id = OLD.id
      and (tenant_id = OLD.tenant_id or (tenant_id is null and OLD.tenant_id is null))
      and valid_to = ores_utility_infinity_timestamp_fn();
