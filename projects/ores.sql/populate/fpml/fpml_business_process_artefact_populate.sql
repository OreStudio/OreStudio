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
 * DQ Artefact FpML Business Process Population Script
 *
 * Populates the dq_business_processes_artefact_tbl with reference data.
 * Dataset: fpml.business_process
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_business_processes_publish_fn() to publish to production.
 */

set schema 'metadata';

-- =============================================================================
-- DQ Artefact FpML Business Process
-- =============================================================================

\echo '--- DQ Artefact FpML Business Process ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from metadata.dq_datasets_tbl
    where code = 'fpml.business_process'
    and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.business_process not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from metadata.dq_business_processes_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into metadata.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Allocation',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for splitting a trade across accounts.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Clearing',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for novating a trade to a central counterparty (with margining) for credit risk mitigation.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Confirmation',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for verifying the terms of a trade.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Execution',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for executing a trade.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Reconciliation',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for comparing representations of a trade or portfolio for the purpose of identifying and resolving discrepancies.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Settlement',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for calculating payment amounts and performing payments as required by the terms of a transaction.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_business_processes_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_business_processes_artefact' as entity, count(*) as count
from metadata.dq_business_processes_artefact_tbl;

select coding_scheme_code, count(*) as count
from metadata.dq_business_processes_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
