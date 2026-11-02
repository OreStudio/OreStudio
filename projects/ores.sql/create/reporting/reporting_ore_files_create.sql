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
 * ORE File Table
 *
 * Reference data naming the files an ORE run reads and writes, one row for each
 * file as the ORE user guide identifies it: the market data, the fixing history, the
 * NPV report, the NPV cube. The code is the guide's name for the file, so it says
 * what the file is and not what a run happened to call it.
 *
 * A file is not a parameter. Several parameters may name the same file: the NPV
 * cube is written by the cubeFile of simulation and read by the cubeFile of xva and
 * pfe. Each of those is a file type, and each file type names its file by this
 * code. A file no parameter names, such as the master input ore.xml or the log, is
 * still a file.
 *
 * The table recognises a file by its name. The typical name is the name ORE gives
 * the file when nothing says otherwise, and the name ORE Studio writes when a run
 * has one file of the kind. The regular expression matches every name a run may
 * give it, so a file a run wrote can be told apart without knowing which parameter
 * named it.
 *
 * The table describes files. It does not hold the content of a file.
 */

create table if not exists "ores_reporting_ore_files_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "category_code" text not null,
    "configuration_type_code" text null,
    "guide_section" text null,
    "origin" text not null,
    "format" text not null,
    "xsd_file" text null,
    "typical_filename" text not null,
    "filename_regex" text not null,
    "description" text not null,
    "display_order" integer not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, code, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("code" <> ''),
    check ("name" <> ''),
    check ("origin" in ('supplied', 'produced', 'packaged')),
    check ("format" in ('xml', 'csv', 'csv_gz', 'txt', 'dat', 'json', 'json_gz', 'tar_gz')),
    check ("xsd_file" is null or "format" = 'xml'),
    check ("typical_filename" ~ '^[A-Za-z0-9_.-]+$'),
    check ("filename_regex" ~ '^\^.*\$$'),
    check ("typical_filename" ~ "filename_regex")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists ore_files_version_uniq_idx
on "ores_reporting_ore_files_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ore_files_code_uniq_idx
on "ores_reporting_ore_files_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ore_files_tenant_idx
on "ores_reporting_ore_files_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_ore_files_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate category_code (soft FK to ores_reporting_ore_file_categories_tbl)
    if not exists (
        select 1 from ores_reporting_ore_file_categories_tbl
        where tenant_id = NEW.tenant_id
          and code = NEW.category_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid category_code: %. No active ORE file category found with this code.', NEW.category_code
            using errcode = '23503';
    end if;

    -- Validate configuration_type_code (optional soft FK to ores_reporting_configuration_types_tbl)
    if NEW.configuration_type_code is not null then
        if not exists (
            select 1 from ores_reporting_configuration_types_tbl
            where tenant_id = NEW.tenant_id
              and code = NEW.configuration_type_code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid configuration_type_code: %. No active configuration type found with this code.', NEW.configuration_type_code
                using errcode = '23503';
        end if;
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_reporting_ore_files_tbl"
    where tenant_id = NEW.tenant_id
      and code = NEW.code
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
                    'ore_file',
                    'code');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'ore_file',
                'code',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_reporting_ore_files_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and code = NEW.code
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

create or replace trigger ores_reporting_ore_files_insert_trg
before insert on "ores_reporting_ore_files_tbl"
for each row execute function ores_reporting_ore_files_insert_fn();

create or replace rule ores_reporting_ore_files_delete_rule as
on delete to "ores_reporting_ore_files_tbl" do instead (
    update "ores_reporting_ore_files_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for ORE File
-- =============================================================================
alter table ores_reporting_ore_files_tbl enable row level security;

drop policy if exists ore_files_tbl_tenant_isolation_policy
    on ores_reporting_ore_files_tbl;

create policy ore_files_tbl_tenant_isolation_policy
on ores_reporting_ore_files_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
