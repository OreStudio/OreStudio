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
 * DQ Artefact FpML Cftc Entity Classification Population Script
 *
 * Populates the dq_entity_classifications_artefact_tbl with reference data.
 * Dataset: fpml.cftc_entity_classification
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_entity_classifications() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Cftc Entity Classification
-- =============================================================================

\echo '--- DQ Artefact FpML Cftc Entity Classification ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.cftc_entity_classification'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.cftc_entity_classification not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_entity_classifications_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CommodityPool',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A commodity pool as defined in CFTC CEA § 2(h)(7)(C).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EmployeeBenefitPlan',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'An employee benefit plan as defined in paragraphs (3) and (32) of section 1002 of title 29 of the Commodity Exchange Act (CEA).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FinancialSectorPerson',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A person predominantly engaged in activities that are in the business of banking, or in activities that are financial in nature, as defined in section 1843(k) of title 12 of the Commodity Exchange Act (CEA).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MSBSP',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A major security based swap participant as defined in CFTC CEA § 2(h)(7)(C).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MSP',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A major swap participant as defined in CFTC CEA § 2(h)(7)(C).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'None',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'None of the other codes apply.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PrivateFund',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A private fund as defined in section 80b-2(a) of title 15 of the Commodity Exchange Act (CEA).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SBSD',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A security-based swap dealer as defined in CFTC CEA § 2(h)(7)(C).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SD',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A swap dealer as defined in CFTC CEA § 2(h)(7)(C).'
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
from ores.dq_entity_classifications_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_entity_classifications_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
