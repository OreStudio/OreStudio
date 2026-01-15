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
-- Helper Function
-- =============================================================================

create or replace function ores.upsert_dq_dataset(
    p_subject_area_name text,
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
    v_subject_area_id uuid;
    v_methodology_id uuid;
begin
    -- Get related IDs (only for tables that still use UUID PKs)
    select id into v_subject_area_id from ores.dq_subject_area_tbl where name = p_subject_area_name and valid_to = ores.utility_infinity_timestamp_fn();
    select id into v_methodology_id from ores.dq_methodology_tbl where name = p_methodology_name and valid_to = ores.utility_infinity_timestamp_fn();

    if v_subject_area_id is null then raise exception 'Subject area not found: %', p_subject_area_name; end if;
    if v_methodology_id is null then raise exception 'Methodology not found: %', p_methodology_name; end if;

    if not exists (
        select 1 from ores.dq_dataset_tbl
        where name = p_name
          and subject_area_id = v_subject_area_id
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_dataset_tbl (
            id, version, subject_area_id, origin_code, nature_code, treatment_code, methodology_id,
            name, description, source_system_id, business_context,
            upstream_derivation_id, lineage_depth, as_of_date, ingestion_timestamp, license_info,
            modified_by, change_reason_code, change_commentary,
            valid_from, valid_to
        )
        values (
            gen_random_uuid(), 0, v_subject_area_id, p_origin_code, p_nature_code, p_treatment_code, v_methodology_id,
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

-- =============================================================================
-- Seed Data
-- =============================================================================

select ores.upsert_dq_dataset(
    'Countries',
    'Source',
    'Actual',
    'Raw',
    'Wikipedia ISO 3166 Extraction',
    'ISO 3166 Countries from Wikipedia',
    'ISO 3166-1 country codes and names from Wikipedia',
    'WIKIPEDIA',
    'Reference data for country codes',
    current_date,
    'CC BY-SA 3.0'
);

select ores.upsert_dq_dataset(
    'Countries',
    'Source',
    'Actual',
    'Raw',
    'GitHub Flag Icons Download',
    'Country Flags from lipis/flag-icons',
    'SVG country flags from lipis/flag-icons repository',
    'GITHUB',
    'Visual assets for countries',
    '2025-12-20'::date,
    'MIT'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_dataset(text, text, text, text, text, text, text, text, text, date, text);

-- =============================================================================
-- Summary
-- =============================================================================

select 'dq_dataset' as entity, count(*) as count
from ores.dq_dataset_tbl
where valid_to = ores.utility_infinity_timestamp_fn();
