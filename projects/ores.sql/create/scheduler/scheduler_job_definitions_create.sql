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
 * OreStudio's metadata overlay for pg_cron jobs. Each row is the OreStudio
 * "shadow" of a cron.job entry, adding tenant/party isolation, human-readable
 * description, audit trail, and a soft-pause mechanism (is_active).
 *
 * The cron_job_id column links to cron.job.jobid and is set by the
 * cron_scheduler service after calling cron.schedule(). It is NULL when the
 * job has not been scheduled yet or has been paused (unscheduled from pg_cron
 * while the definition is retained).
 *
 * Hand-crafted: the cron_job_id bigint column and is_active integer cannot be
 * expressed by the standard domain_entity template.
 */

create table if not exists "ores_scheduler_job_definitions_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "cron_job_id" bigint null,
    "job_name" text not null,
    "description" text not null default '',
    "command" text not null,
    "schedule_expression" text not null,
    "database_name" text not null,
    "is_active" integer not null default 1,
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
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("job_name" <> ''),
    check ("command" <> ''),
    check ("schedule_expression" <> '')
);

-- Unique cron_job_id among active records (NULL allowed for paused/unscheduled)
create unique index if not exists ores_scheduler_job_definitions_cron_job_id_uniq_idx
on "ores_scheduler_job_definitions_tbl" (cron_job_id)
where valid_to = ores_utility_infinity_timestamp_fn() and cron_job_id is not null;

-- Unique job_name per tenant among active records
create unique index if not exists ores_scheduler_job_definitions_name_uniq_idx
on "ores_scheduler_job_definitions_tbl" (tenant_id, job_name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_scheduler_job_definitions_version_uniq_idx
on "ores_scheduler_job_definitions_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Current record uniqueness
create unique index if not exists ores_scheduler_job_definitions_id_uniq_idx
on "ores_scheduler_job_definitions_tbl" (tenant_id, id)
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
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate party_id (soft FK to ores_refdata_parties_tbl)
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

    -- Version management
    select version into current_version
    from "ores_scheduler_job_definitions_tbl"
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

        update "ores_scheduler_job_definitions_tbl"
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
    NEW.performed_by = coalesce(ores_iam_current_actor_fn(), current_user);

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
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
