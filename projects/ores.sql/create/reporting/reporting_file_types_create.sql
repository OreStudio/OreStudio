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
 * File Type Table
 *
 * Reference data naming the files an ORE run reads and writes. A file type is one
 * parameter of the run document that carries a file name: a Setup parameter such as
 * conventionsFile, or the parameter of an analytic such as the outputFileName of npv.
 * The table gives each its well known name, so a configuration never repeats a file
 * name and the export looks the name up when it writes a file.
 *
 * The names come from the shipped ORE samples. Each is the most common name the samples
 * give the file, and sample_count out of sample_total records how many use it.
 * Names that embed a date are set aside, because a date is a value of the run.
 *
 * A name may be a pattern with a key, for example portfolio_{{BOOK_ID}}.xml, for a file
 * that a run splits. The table can hold the pattern. Splitting is a later task.
 *
 * The table lists names and patterns. It does not hold the content of a file.
 */

create table if not exists "ores_reporting_file_types_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "analytic_type_code" text null,
    "ore_parameter" text not null,
    "kind" text not null,
    "configuration_type_code" text null,
    "name_pattern" text not null,
    "sample_count" integer not null,
    "sample_total" integer not null,
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
    check ("kind" in ('input_document', 'data_file', 'output_file', 'intermediate_file')),
    check ("name_pattern" ~ '^[A-Za-z0-9_.-]*(\{\{[A-Z][A-Z0-9_]*\}\}[A-Za-z0-9_.-]*)*$'),
    check ("sample_count" >= 0 and "sample_count" <= "sample_total")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists file_types_version_uniq_idx
on "ores_reporting_file_types_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists file_types_code_uniq_idx
on "ores_reporting_file_types_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists file_types_tenant_idx
on "ores_reporting_file_types_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_file_types_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate analytic_type_code (optional soft FK to ores_reporting_analytic_types_tbl)
    if NEW.analytic_type_code is not null then
        if not exists (
            select 1 from ores_reporting_analytic_types_tbl
            where tenant_id = NEW.tenant_id
              and code = NEW.analytic_type_code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid analytic_type_code: %. No active analytic type found with this code.', NEW.analytic_type_code
                using errcode = '23503';
        end if;
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
    from "ores_reporting_file_types_tbl"
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
                    'file_type',
                    'code');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'file_type',
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
        update "ores_reporting_file_types_tbl"
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

create or replace trigger ores_reporting_file_types_insert_trg
before insert on "ores_reporting_file_types_tbl"
for each row execute function ores_reporting_file_types_insert_fn();

create or replace rule ores_reporting_file_types_delete_rule as
on delete to "ores_reporting_file_types_tbl" do instead (
    update "ores_reporting_file_types_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for File Type
-- =============================================================================
alter table ores_reporting_file_types_tbl enable row level security;

drop policy if exists file_types_tbl_tenant_isolation_policy
    on ores_reporting_file_types_tbl;

create policy file_types_tbl_tenant_isolation_policy
on ores_reporting_file_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
