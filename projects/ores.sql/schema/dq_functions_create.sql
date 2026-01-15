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

set schema 'ores';

-- =============================================================================
-- Data Quality Helper Functions
-- =============================================================================

-- Upsert Methodology
create or replace function ores.upsert_dq_methodology(
    p_name text,
    p_description text,
    p_logic_reference text default null,
    p_implementation_details text default null
) returns void as $$
declare
    v_id uuid;
begin
    if not exists (
        select 1 from ores.dq_methodology_tbl
        where name = p_name
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_methodology_tbl (
            id, version, name, description, logic_reference, implementation_details,
            modified_by, change_reason_code, change_commentary,
            valid_from, valid_to
        )
        values (
            gen_random_uuid(), 0, p_name, p_description, p_logic_reference, p_implementation_details,
            'system', 'system.new_record', 'System seed data',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created dq_methodology: %', p_name;
    else
        raise notice 'dq_methodology already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

-- Upsert Dataset
create or replace function ores.upsert_dq_dataset(
    p_subject_area_name text,
    p_domain_name text,
    p_origin_code text,
    p_nature_code text,
    p_treatment_code text,
    p_methodology_name text,
    p_name text,
    p_description text,
    p_source_system_id text,
    p_business_context text,
    p_as_of_date date,
    p_license_info text default null
) returns void as $$
declare
    v_methodology_id uuid;
begin
    -- Get methodology ID (only table that still uses UUID PK)
    select id into v_methodology_id from ores.dq_methodology_tbl where name = p_methodology_name and valid_to = ores.utility_infinity_timestamp_fn();

    if v_methodology_id is null then raise exception 'Methodology not found: %', p_methodology_name; end if;

    if not exists (
        select 1 from ores.dq_dataset_tbl
        where name = p_name
          and subject_area_name = p_subject_area_name
          and domain_name = p_domain_name
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_dataset_tbl (
            id, version, subject_area_name, domain_name, origin_code, nature_code, treatment_code, methodology_id,
            name, description, source_system_id, business_context,
            upstream_derivation_id, lineage_depth, as_of_date, ingestion_timestamp, license_info,
            modified_by, change_reason_code, change_commentary,
            valid_from, valid_to
        )
        values (
            gen_random_uuid(), 0, p_subject_area_name, p_domain_name, p_origin_code, p_nature_code, p_treatment_code, v_methodology_id,
            p_name, p_description, p_source_system_id, p_business_context,
            null, 0, p_as_of_date, current_timestamp, p_license_info,
            'system', 'system.new_record', 'System seed data',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created dq_dataset: %', p_name;
    else
        raise notice 'dq_dataset already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

-- Upsert Images Artefact (Snapshot)
create or replace function ores.upsert_dq_images_artefact(
    p_dataset_name text
) returns void as $$
declare
    v_dataset_id uuid;
    v_count integer;
begin
    -- 1. Get the dataset ID
    select id into v_dataset_id
    from ores.dq_dataset_tbl
    where name = p_dataset_name
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset "%" not found.', p_dataset_name;
    end if;

    -- 2. Clear existing data for this dataset
    delete from ores.dq_images_artefact_tbl
    where dataset_id = v_dataset_id;

    -- 3. Insert data from assets_images_tbl
    insert into ores.dq_images_artefact_tbl (
        dataset_id,
        image_id,
        version,
        key,
        description,
        svg_data
    )
    select
        v_dataset_id,
        image_id,
        version,
        key,
        description,
        svg_data
    from ores.assets_images_tbl
    where valid_to = ores.utility_infinity_timestamp_fn();

    get diagnostics v_count = row_count;
    raise notice 'Inserted % records into dq_images_artefact_tbl for dataset %', v_count, p_dataset_name;
end;
$$ language plpgsql;

-- Upsert Tags Artefact (Snapshot)
create or replace function ores.upsert_dq_tags_artefact(
    p_dataset_name text
) returns void as $$
declare
    v_dataset_id uuid;
    v_count integer;
begin
    -- 1. Get the dataset ID
    select id into v_dataset_id
    from ores.dq_dataset_tbl
    where name = p_dataset_name
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset "%" not found.', p_dataset_name;
    end if;

    -- 2. Clear existing data for this dataset
    delete from ores.dq_tags_artefact_tbl
    where dataset_id = v_dataset_id;

    -- 3. Insert data from assets_tags_tbl
    insert into ores.dq_tags_artefact_tbl (
        dataset_id,
        tag_id,
        version,
        name,
        description
    )
    select
        v_dataset_id,
        tag_id,
        version,
        name,
        description
    from ores.assets_tags_tbl
    where valid_to = ores.utility_infinity_timestamp_fn();

    get diagnostics v_count = row_count;
    raise notice 'Inserted % records into dq_tags_artefact_tbl for dataset %', v_count, p_dataset_name;
end;
$$ language plpgsql;

-- Upsert Image Tags Artefact (Snapshot)
create or replace function ores.upsert_dq_image_tags_artefact(
    p_dataset_name text
) returns void as $$
declare
    v_dataset_id uuid;
    v_count integer;
begin
    -- 1. Get the dataset ID
    select id into v_dataset_id
    from ores.dq_dataset_tbl
    where name = p_dataset_name
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset "%" not found.', p_dataset_name;
    end if;

    -- 2. Clear existing data for this dataset
    delete from ores.dq_image_tags_artefact_tbl
    where dataset_id = v_dataset_id;

    -- 3. Insert data from assets_image_tags_tbl
    insert into ores.dq_image_tags_artefact_tbl (
        dataset_id,
        image_id,
        tag_id
    )
    select
        v_dataset_id,
        it.image_id,
        it.tag_id
    from ores.assets_image_tags_tbl it
    where it.valid_to = ores.utility_infinity_timestamp_fn();

    get diagnostics v_count = row_count;
    raise notice 'Inserted % records into dq_image_tags_artefact_tbl for dataset %', v_count, p_dataset_name;
end;
$$ language plpgsql;
