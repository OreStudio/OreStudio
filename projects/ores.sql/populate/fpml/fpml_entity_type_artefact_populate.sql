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
 * DQ Artefact FpML Entity Type Population Script
 *
 * Populates the dq_entity_classifications_artefact_tbl with reference data.
 * Dataset: fpml.entity_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_entity_classifications() to publish to production.
 */


-- =============================================================================
-- DQ Artefact FpML Entity Type
-- =============================================================================

\echo '--- DQ Artefact FpML Entity Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'fpml.entity_type'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.entity_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores_dq_entity_classifications_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Asian',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Asian.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AustralianAndNewZealand',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Australian and New Zealand.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EuropeanEmergingMarkets',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of European Emerging Markets.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Japanese',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Japanese.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NorthAmericanHighYield',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of North American High Yield.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NorthAmericanInsurance',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of North American Insurance.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NorthAmericanInvestmentGrade',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of North American Investment Grade.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Singaporean',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Singaporean.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'WesternEuropean',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Western European.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'WesternEuropeanInsurance',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Western European Insurance.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_entity_classifications_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_entity_classifications_artefact' as entity, count(*) as count
from ores_dq_entity_classifications_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores_dq_entity_classifications_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
