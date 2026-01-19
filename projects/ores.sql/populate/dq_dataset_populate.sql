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
-- Helper Functions
-- =============================================================================

-- Helper function to create a tag for a dataset
create or replace function ores.upsert_dq_tag(
    p_dataset_name text,
    p_subject_area_name text,
    p_domain_name text,
    p_tag_name text,
    p_tag_description text
) returns void as $$
declare
    v_dataset_id uuid;
begin
    -- Get dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where name = p_dataset_name
      and subject_area_name = p_subject_area_name
      and domain_name = p_domain_name
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: name=%, subject_area=%, domain=%',
            p_dataset_name, p_subject_area_name, p_domain_name;
    end if;

    -- Delete existing tag with same name for this dataset (idempotency)
    delete from ores.dq_tags_artefact_tbl
    where dataset_id = v_dataset_id and name = p_tag_name;

    -- Insert the tag
    insert into ores.dq_tags_artefact_tbl (
        dataset_id, tag_id, version, name, description
    ) values (
        v_dataset_id, gen_random_uuid(), 0, p_tag_name, p_tag_description
    );

    raise notice 'Created dq_tag: % for dataset %', p_tag_name, p_dataset_name;
end;
$$ language plpgsql;

-- Helper function to create a dataset
create or replace function ores.upsert_dq_datasets(
    p_catalog_name text,
    p_subject_area_name text,
    p_domain_name text,
    p_coding_scheme_code text,
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
    select id into v_methodology_id from ores.dq_methodologies_tbl where name = p_methodology_name and valid_to = ores.utility_infinity_timestamp_fn();

    if v_methodology_id is null then raise exception 'Methodology not found: %', p_methodology_name; end if;

    if not exists (
        select 1 from ores.dq_datasets_tbl
        where name = p_name
          and subject_area_name = p_subject_area_name
          and domain_name = p_domain_name
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_datasets_tbl (
            id, version, catalog_name, subject_area_name, domain_name, coding_scheme_code,
            origin_code, nature_code, treatment_code, methodology_id,
            name, description, source_system_id, business_context,
            upstream_derivation_id, lineage_depth, as_of_date, ingestion_timestamp, license_info,
            modified_by, change_reason_code, change_commentary,
            valid_from, valid_to
        )
        values (
            gen_random_uuid(), 0, p_catalog_name, p_subject_area_name, p_domain_name, p_coding_scheme_code,
            p_origin_code, p_nature_code, p_treatment_code, v_methodology_id,
            p_name, p_description, p_source_system_id, p_business_context,
            null, 0, p_as_of_date, current_timestamp, p_license_info,
            'system', 'system.new_record', 'System seed data',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created dq_datasets: %', p_name;
    else
        raise notice 'dq_datasets already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Seed Data
-- =============================================================================

select ores.upsert_dq_datasets(
    'ISO Standards',
    'Countries',
    'Reference Data',
    'ISO_3166_1_ALPHA_2',
    'Source',
    'Actual',
    'Raw',
    'Wikipedia ISO 3166 Extraction',
    'ISO 3166 Country Codes',
    'ISO 3166-1 alpha-2 country codes and official names.',
    'WIKIPEDIA',
    'Reference data for country codes',
    current_date,
    'CC BY-SA 3.0'
);

select ores.upsert_dq_datasets(
    'ISO Standards',
    'Countries',
    'Reference Data',
    'ISO_3166_1_ALPHA_2',
    'Source',
    'Actual',
    'Raw',
    'GitHub Flag Icons Download',
    'Country Flag Images',
    'SVG flag images for each ISO 3166-1 country.',
    'GITHUB',
    'Visual assets for countries',
    '2025-12-20'::date,
    'MIT'
);

select ores.upsert_dq_tag(
    'Country Flag Images',
    'Countries',
    'Reference Data',
    'flag',
    'Country and region flag images'
);

select ores.upsert_dq_datasets(
    'Cryptocurrency',
    'Cryptocurrencies',
    'Reference Data',
    'NONE',
    'Source',
    'Actual',
    'Raw',
    'GitHub Cryptocurrency Icons Download',
    'Cryptocurrency Icon Images',
    'SVG icon images for major cryptocurrencies.',
    'GITHUB',
    'Visual assets for cryptocurrencies',
    '2025-01-15'::date,
    'CC0 1.0 Universal'
);

select ores.upsert_dq_tag(
    'Cryptocurrency Icon Images',
    'Cryptocurrencies',
    'Reference Data',
    'cryptocurrency',
    'Cryptocurrency icon images'
);

select ores.upsert_dq_datasets(
    'ISO Standards',
    'Currencies',
    'Reference Data',
    'ISO_4217',
    'Source',
    'Actual',
    'Raw',
    'Wikipedia ISO 4217 Extraction',
    'ISO 4217 Currency Codes',
    'ISO 4217 alphabetic and numeric currency codes.',
    'WIKIPEDIA',
    'Reference data for currency codes',
    current_date,
    'CC BY-SA 3.0'
);

select ores.upsert_dq_tag(
    'ISO 4217 Currency Codes',
    'Currencies',
    'Reference Data',
    'currency',
    'Currency reference data'
);

select ores.upsert_dq_datasets(
    'FpML Standards',
    'Currencies',
    'Reference Data',
    'FPML_NON_ISO_CURRENCY',
    'Source',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Non-ISO Currency Codes',
    'Non-standard currency codes for derivatives (offshore CNH/CNT, historical MCF/SML/VAL).',
    'FPML',
    'Reference data for non-ISO currency codes used in derivatives trading',
    '2023-11-03'::date,
    null
);

select ores.upsert_dq_tag(
    'FpML Non-ISO Currency Codes',
    'Currencies',
    'Reference Data',
    'currency',
    'Non-ISO currency reference data'
);

select ores.upsert_dq_datasets(
    'Cryptocurrency',
    'Cryptocurrencies',
    'Reference Data',
    'NONE',
    'Source',
    'Actual',
    'Raw',
    'GitHub Cryptocurrencies JSON Download',
    'Cryptocurrency Reference Data',
    'Cryptocurrency symbols, names, and metadata.',
    'GITHUB',
    'Reference data for cryptocurrency codes',
    current_date,
    'MIT'
);

select ores.upsert_dq_tag(
    'Cryptocurrency Reference Data',
    'Cryptocurrencies',
    'Reference Data',
    'cryptocurrency',
    'Cryptocurrency reference data'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_datasets(text, text, text, text, text, text, text, text, text, text, text, text, date, text);
drop function ores.upsert_dq_tag(text, text, text, text, text);

-- =============================================================================
-- Summary
-- =============================================================================

select 'dq_datasets' as entity, count(*) as count
from ores.dq_datasets_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'dq_tags_artefact', count(*)
from ores.dq_tags_artefact_tbl;
