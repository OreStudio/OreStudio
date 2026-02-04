/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
 * Data Quality Scheme Table
 *
 * A scheme defines a coding or identification standard used to identify
 * entities such as parties, currencies, or other domain objects. Based on
 * the FPML coding-scheme concept.
 *
 * Each scheme belongs to a subject area within the DQ hierarchy, establishing
 * what type of entity the scheme identifies.
 *
 * Examples:
 * - LEI (Legal Entity Identifier, ISO 17442) for party identification
 * - BIC (Business Identifier Code, ISO 9362) for financial institutions
 * - ISO 4217 for currency codes
 * - ISO 3166 for country codes
 */

create table if not exists "ores_dq_coding_schemes_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "authority_type" text not null,
    "subject_area_name" text not null,
    "domain_name" text not null,
    "uri" text,
    "description" text not null,
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
    check ("code" <> '')
);

-- Unique indexes for current records
create unique index if not exists ores_dq_coding_schemes_version_uniq_idx
on "ores_dq_coding_schemes_tbl" (code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_dq_coding_schemes_code_uniq_idx
on "ores_dq_coding_schemes_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_dq_coding_schemes_tenant_idx
on "ores_dq_coding_schemes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for looking up schemes by subject area
create index if not exists ores_dq_coding_schemes_subject_area_idx
on "ores_dq_coding_schemes_tbl" (subject_area_name, domain_name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for looking up schemes by URI
create index if not exists ores_dq_coding_schemes_uri_idx
on "ores_dq_coding_schemes_tbl" (uri)
where valid_to = ores_utility_infinity_timestamp_fn() and uri is not null;

-- Index for looking up schemes by authority type
create index if not exists ores_dq_coding_schemes_authority_type_idx
on "ores_dq_coding_schemes_tbl" (authority_type)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_dq_coding_schemes_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Validate authority_type FK
    if not exists (
        select 1 from ores_dq_coding_scheme_authority_types_tbl
        where code = NEW.authority_type
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid authority_type: %. Authority type must exist.', NEW.authority_type
        using errcode = '23503';
    end if;

    -- Validate subject_area/domain FK
    if not exists (
        select 1 from ores_dq_subject_areas_tbl
        where name = NEW.subject_area_name
        and domain_name = NEW.domain_name
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid subject_area_name/domain_name: %/%. Subject area must exist.', NEW.subject_area_name, NEW.domain_name
        using errcode = '23503';
    end if;

    select version into current_version
    from "ores_dq_coding_schemes_tbl"
    where tenant_id = NEW.tenant_id
      and code = NEW.code
      and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        -- This insert is an update. Check version and increment.
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        -- Close the old record.
        update "ores_dq_coding_schemes_tbl"
        set valid_to = current_timestamp
        where tenant_id = NEW.tenant_id
          and code = NEW.code
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        -- This is a new record.
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();

    if NEW.modified_by is null or NEW.modified_by = '' then
        NEW.modified_by = current_user;
    end if;

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_dq_coding_schemes_insert_trg
before insert on "ores_dq_coding_schemes_tbl"
for each row execute function ores_dq_coding_schemes_insert_fn();

create or replace rule ores_dq_coding_schemes_delete_rule as
on delete to "ores_dq_coding_schemes_tbl" do instead
    update "ores_dq_coding_schemes_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
