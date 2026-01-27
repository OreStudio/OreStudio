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
 * Populates the dq_local_jurisdictions_artefact_tbl with reference data.
 * Dataset: fpml.local_jurisdiction
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_local_jurisdictions_fn() to publish to production.
 */

set schema 'metadata';

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
    from metadata.dq_datasets_tbl
    where code = 'fpml.local_jurisdiction'
    and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.local_jurisdiction not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from metadata.dq_local_jurisdictions_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Afghanistan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Afghan Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Applicable',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Follows Local Jurisdiction as per MCA to this Transaction.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Australia',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Australian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'China',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Chinese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HongKong',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Hong Kong Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'India',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Indian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Indonesia',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Indonesian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Japan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Japanese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Korea',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Korean Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Malaysia',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Malaysian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NewZealand',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'New Zealand Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NotApplicable',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'No Local Jurisdiction applies to this Transaction.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Pakistan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Pakistani Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Philippines',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Philippine Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Singapore',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Singaporean Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Taiwan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Taiwanese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Thailand',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Thai Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Vietnam',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Vietnamese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_local_jurisdictions_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_local_jurisdictions_artefact' as entity, count(*) as count
from metadata.dq_local_jurisdictions_artefact_tbl;

select coding_scheme_code, count(*) as count
from metadata.dq_local_jurisdictions_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
