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
 * Data Quality Countries Dataset Population Script
 *
 * Creates a dataset entry for ISO 3166 countries data from Wikipedia.
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- Helper function to insert a data quality methodology if it doesn't exist
create or replace function ores.upsert_dq_methodology(
    p_name text,
    p_description text,
    p_logic_reference text default null,
    p_implementation_details text default null
) returns uuid as $$
declare
    v_id uuid;
begin
    select id into v_id
    from ores.dq_methodology_tbl
    where name = p_name and valid_to = ores.utility_infinity_timestamp_fn();

    if v_id is null then
        v_id := gen_random_uuid();
        insert into ores.dq_methodology_tbl (
            id, version, name, description, logic_reference, implementation_details,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            v_id, 0, p_name, p_description, p_logic_reference, p_implementation_details,
            'system', 'system.new_record', 'System seed data - data quality methodology',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality methodology: %', p_name;
    else
        raise notice 'Data quality methodology already exists: %', p_name;
    end if;

    return v_id;
end;
$$ language plpgsql;

-- Helper function to insert a data quality dataset if it doesn't exist
create or replace function ores.upsert_dq_dataset(
    p_subject_area_name text,
    p_domain_name text,
    p_origin_code text,
    p_nature_code text,
    p_treatment_code text,
    p_methodology_id uuid,
    p_name text,
    p_description text,
    p_source_system_id text,
    p_business_context text,
    p_as_of_date date,
    p_license_info text default null
) returns uuid as $$
declare
    v_id uuid;
begin
    -- Check if dataset already exists
    select id into v_id
    from ores.dq_dataset_tbl
    where name = p_name
    and subject_area_name = p_subject_area_name
    and domain_name = p_domain_name
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_id is null then
        v_id := gen_random_uuid();
        insert into ores.dq_dataset_tbl (
            id, version, subject_area_name, domain_name, origin_code, nature_code, treatment_code, methodology_id,
            name, description, source_system_id, business_context,
            upstream_derivation_id, lineage_depth, as_of_date, ingestion_timestamp, license_info,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            v_id, 0, p_subject_area_name, p_domain_name, p_origin_code, p_nature_code, p_treatment_code, p_methodology_id,
            p_name, p_description, p_source_system_id, p_business_context,
            null, 0, p_as_of_date, current_timestamp, p_license_info,
            'system', 'system.new_record', 'System seed data - data quality dataset',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality dataset: %', p_name;
    else
        raise notice 'Data quality dataset already exists: %', p_name;
    end if;

    return v_id;
end;
$$ language plpgsql;

-- =============================================================================
-- Create Dataset for ISO 3166 Countries from Wikipedia
-- =============================================================================

\echo '--- Creating Dataset for ISO 3166 Countries from Wikipedia ---'

do $$
declare
    v_methodology_id uuid;
    v_dataset_id uuid;
begin
    -- Create methodology for Wikipedia data extraction
    v_methodology_id := ores.upsert_dq_methodology(
        'Wikipedia ISO 3166 Extraction',
        'Data extracted from Wikipedia page listing ISO 3166 country codes',
        'https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes',
        'Manual extraction of ISO 3166-1 alpha-2, alpha-3, and numeric codes with country names'
    );

    -- Create dataset for ISO 3166 countries
    v_dataset_id := ores.upsert_dq_dataset(
        'Countries',                    -- subject area
        'Reference Data',               -- domain
        'Source',                       -- origin
        'Actual',                       -- nature
        'Raw',                          -- treatment
        v_methodology_id,               -- methodology
        'ISO 3166 Countries from Wikipedia',  -- name
        'ISO 3166-1 country codes and names from Wikipedia',  -- description
        'WIKIPEDIA',                    -- source system ID
        'Reference data for country codes',  -- business context
        current_date,                   -- as-of date
        'CC BY-SA 3.0'                 -- license info
    );

    -- Store the dataset ID for potential use in artifact population
    raise notice 'Dataset ID for ISO 3166 Countries: %', v_dataset_id;
end $$;

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_methodology(text, text, text, text);
drop function ores.upsert_dq_dataset(text, text, text, text, text, uuid, text, text, text, text, date, text);