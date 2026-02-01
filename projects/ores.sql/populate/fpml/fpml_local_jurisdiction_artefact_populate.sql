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
 * DQ Artefact FpML Local Jurisdiction Population Script
 *
 * Populates the ores_dq_local_jurisdictions_artefact_tbl with reference data.
 * Dataset: fpml.local_jurisdiction
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use ores_dq_populate_local_jurisdictions() to publish to production.
 */


-- =============================================================================
-- DQ Artefact FpML Local Jurisdiction
-- =============================================================================

\echo '--- DQ Artefact FpML Local Jurisdiction ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'fpml.local_jurisdiction'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.local_jurisdiction not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores_dq_local_jurisdictions_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Afghanistan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Afghan Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Applicable',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Follows Local Jurisdiction as per MCA to this Transaction.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Australia',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Australian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'China',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Chinese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'HongKong',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Hong Kong Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'India',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Indian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Indonesia',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Indonesian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Japan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Japanese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Korea',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Korean Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Malaysia',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Malaysian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NewZealand',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'New Zealand Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NotApplicable',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'No Local Jurisdiction applies to this Transaction.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Pakistan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Pakistani Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Philippines',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Philippine Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Singapore',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Singaporean Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Taiwan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Taiwanese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Thailand',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Thai Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores_dq_local_jurisdictions_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'Vietnam',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Vietnamese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into ores_dq_local_jurisdictions_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_local_jurisdictions_artefact' as entity, count(*) as count
from ores_dq_local_jurisdictions_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores_dq_local_jurisdictions_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
