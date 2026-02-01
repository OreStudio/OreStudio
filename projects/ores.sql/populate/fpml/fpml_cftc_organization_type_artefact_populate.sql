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
 * DQ Artefact FpML Cftc Organization Type Population Script
 *
 * Populates the ores_dq_entity_classifications_artefact_tbl with reference data.
 * Dataset: fpml.cftc_organization_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use ores_dq_populate_entity_classifications() to publish to production.
 */


-- =============================================================================
-- DQ Artefact FpML Cftc Organization Type
-- =============================================================================

\echo '--- DQ Artefact FpML Cftc Organization Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'fpml.cftc_organization_type'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.cftc_organization_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores_dq_entity_classifications_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Agency',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'An agency as defined in 5 U.S.C. 551(1), a federal instrumentality, or a federal authority.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CharteredPursuantToFederalLaw',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'An entity chartered pursuant to federal law after formation (example: an organization listed in title 36 of the U.S. Code).'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'EstablishedByFederalEntity',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'An entity that was established by, or at the direction of, one or more of the entities listed in clause (1), or has an ultimate parent listed in its LEI reference data that is an entity listed in clause (1) or in the first part of this clause (2).'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'FederallyFundedResearchAndDevelopmentCenter',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'A federally funded research and development center on the master list referenced in 48 CFR 35.017-6.'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GovernmentCorporation',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'A government corporation (examples: as such term is defined in 5 U.S.C. 103(1) or in 31 U.S.C. 9101).'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GovernmentSponsoredEnterprise',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'A government-sponsored enterprise (example: as such term is defined in 2 U.S.C. 622(8)).'
    );
    v_count := v_count + 1;
    insert into ores_dq_entity_classifications_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USCListedExecutiveDepartment',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'An executive department listed in 5 U.S.C. 101.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into ores_dq_entity_classifications_artefact_tbl', v_count;
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
