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

-- =============================================================================
-- Central dataset registry with full lineage.
-- Tracks provenance via upstream_derivation_id.
-- lineage_depth auto-calculated from hierarchy.
-- =============================================================================

create table if not exists "ores"."dq_datasets_tbl" (
    "id" uuid not null,
    "version" integer not null,
    "code" text not null,
    "catalog_name" text,
    "subject_area_name" text not null,
    "domain_name" text not null,
    "coding_scheme_code" text,
    "origin_code" text not null,
    "nature_code" text not null,
    "treatment_code" text not null,
    "methodology_id" uuid,
    "name" text not null,
    "description" text not null,
    "source_system_id" text not null,
    "business_context" text not null,
    "upstream_derivation_id" uuid,
    "lineage_depth" integer not null,
    "as_of_date" timestamp with time zone not null,
    "ingestion_timestamp" timestamp with time zone not null,
    "license_info" text,
    "artefact_type" text,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, valid_from, valid_to),
    exclude using gist (
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid)
);

create unique index if not exists dq_datasets_name_uniq_idx
on "ores"."dq_datasets_tbl" (name, subject_area_name, domain_name)
where valid_to = ores.utility_infinity_timestamp_fn();

create unique index if not exists dq_datasets_code_uniq_idx
on "ores"."dq_datasets_tbl" (code)
where valid_to = ores.utility_infinity_timestamp_fn();

create unique index if not exists dq_datasets_version_uniq_idx
on "ores"."dq_datasets_tbl" (id, version)
where valid_to = ores.utility_infinity_timestamp_fn();

create or replace function ores.dq_datasets_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate foreign key references
    if NEW.catalog_name is not null and not exists (
        select 1 from ores.dq_catalogs_tbl
        where name = NEW.catalog_name
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid catalog_name: %. Catalog must exist.', NEW.catalog_name
        using errcode = '23503';
    end if;

    if not exists (
        select 1 from ores.dq_subject_areas_tbl
        where name = NEW.subject_area_name
        and domain_name = NEW.domain_name
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid subject_area_name/domain_name: %/%. Subject area must exist.', NEW.subject_area_name, NEW.domain_name
        using errcode = '23503';
    end if;

    if NEW.coding_scheme_code is not null and not exists (
        select 1 from ores.dq_coding_schemes_tbl
        where code = NEW.coding_scheme_code
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid coding_scheme_code: %. Coding scheme must exist.', NEW.coding_scheme_code
        using errcode = '23503';
    end if;

    if not exists (
        select 1 from ores.dq_origin_dimensions_tbl
        where code = NEW.origin_code
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid origin_code: %. Origin dimension must exist.', NEW.origin_code
        using errcode = '23503';
    end if;

    if not exists (
        select 1 from ores.dq_nature_dimensions_tbl
        where code = NEW.nature_code
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid nature_code: %. Nature dimension must exist.', NEW.nature_code
        using errcode = '23503';
    end if;

    if not exists (
        select 1 from ores.dq_treatment_dimensions_tbl
        where code = NEW.treatment_code
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid treatment_code: %. Treatment dimension must exist.', NEW.treatment_code
        using errcode = '23503';
    end if;

    if NEW.methodology_id is not null and not exists (
        select 1 from ores.dq_methodologies_tbl
        where id = NEW.methodology_id
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid methodology_id: %. Methodology must exist.', NEW.methodology_id
        using errcode = '23503';
    end if;

    if NEW.upstream_derivation_id is not null and not exists (
        select 1 from ores.dq_datasets_tbl
        where id = NEW.upstream_derivation_id
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid upstream_derivation_id: %. Upstream dataset must exist.', NEW.upstream_derivation_id
        using errcode = '23503';
    end if;

    if NEW.artefact_type is not null and not exists (
        select 1 from ores.dq_artefact_types_tbl
        where code = NEW.artefact_type
    ) then
        raise exception 'Invalid artefact_type: %. Artefact type must exist.', NEW.artefact_type
        using errcode = '23503';
    end if;

    -- Calculate lineage_depth
    if NEW.upstream_derivation_id is null then
        NEW.lineage_depth := 0;
    else
        select COALESCE((select lineage_depth from ores.dq_datasets_tbl
                         where id = NEW.upstream_derivation_id
                         and valid_to = ores.utility_infinity_timestamp_fn()), 0) + 1
        into NEW.lineage_depth;
    end if;

    -- Version management
    select version into current_version
    from "ores"."dq_datasets_tbl"
    where id = NEW.id
      and valid_to = ores.utility_infinity_timestamp_fn();

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update "ores"."dq_datasets_tbl"
        set valid_to = current_timestamp
        where id = NEW.id
          and valid_to = ores.utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores.utility_infinity_timestamp_fn();

    if NEW.modified_by is null or NEW.modified_by = '' then
        NEW.modified_by = current_user;
    end if;

    NEW.change_reason_code := ores.refdata_validate_change_reason_fn(NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger dq_datasets_insert_trg
before insert on "ores"."dq_datasets_tbl"
for each row execute function ores.dq_datasets_insert_fn();

create or replace rule dq_datasets_delete_rule as
on delete to "ores"."dq_datasets_tbl" do instead
    update "ores"."dq_datasets_tbl"
    set valid_to = current_timestamp
    where id = OLD.id
      and valid_to = ores.utility_infinity_timestamp_fn();
