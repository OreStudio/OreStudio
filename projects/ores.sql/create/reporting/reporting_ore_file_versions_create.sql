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
 * ORE File Version Table
 *
 * The contents of an ORE file as one version of the ORE engine ships it. The engine
 * ships some files itself, the XML schemas above all, and the schema of a version
 * is the one a document of that version must validate against: a schema of another
 * version may accept what the engine refuses, or refuse what it reads.
 *
 * One row holds one file at one engine version, with its digest and its size, so a
 * run's page can hand a person the schema of the engine that ran it. A schema that
 * includes others is complete only with them; the includes are read from the
 * contents, which state them, so the two cannot disagree.
 *
 * The seed loads the files vendored in external/ore for the engine version their
 * manifest states. A later engine version adds rows beside the earlier ones and
 * replaces none of them.
 */

create table if not exists "ores_reporting_ore_file_versions_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "ore_file_code" text not null,
    "engine_version" text not null,
    "content" text not null,
    "sha256" text not null,
    "bytes" integer not null,
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
    check ("engine_version" ~ '^[0-9]+(\.[0-9]+)*$'),
    check ("sha256" ~ '^[0-9a-f]{64}$'),
    check ("bytes" = octet_length("content"))
);

-- Composite natural key: unique combination for active records
create unique index if not exists ore_file_versions_ore_file_code_engine_version_uniq_idx
on "ores_reporting_ore_file_versions_tbl" (tenant_id, ore_file_code, engine_version)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists ore_file_versions_version_uniq_idx
on "ores_reporting_ore_file_versions_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ore_file_versions_id_uniq_idx
on "ores_reporting_ore_file_versions_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ore_file_versions_tenant_idx
on "ores_reporting_ore_file_versions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ore_file_versions_ore_file_versions_file_engine_idx
on "ores_reporting_ore_file_versions_tbl" (tenant_id, ore_file_code, engine_version)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_ore_file_versions_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate ore_file_code (soft FK to ores_reporting_ore_files_tbl)
    if not exists (
        select 1 from ores_reporting_ore_files_tbl
        where tenant_id = NEW.tenant_id
          and code = NEW.ore_file_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid ore_file_code: %. No active ORE file found with this code.', NEW.ore_file_code
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
    from "ores_reporting_ore_file_versions_tbl"
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
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'ore_file_version',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'ore_file_version',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_reporting_ore_file_versions_tbl"
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
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_reporting_ore_file_versions_insert_trg
before insert on "ores_reporting_ore_file_versions_tbl"
for each row execute function ores_reporting_ore_file_versions_insert_fn();

create or replace rule ores_reporting_ore_file_versions_delete_rule as
on delete to "ores_reporting_ore_file_versions_tbl" do instead (
    update "ores_reporting_ore_file_versions_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for ORE File Version
-- =============================================================================
alter table ores_reporting_ore_file_versions_tbl enable row level security;

drop policy if exists ore_file_versions_tbl_tenant_isolation_policy
    on ores_reporting_ore_file_versions_tbl;

create policy ore_file_versions_tbl_tenant_isolation_policy
on ores_reporting_ore_file_versions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
