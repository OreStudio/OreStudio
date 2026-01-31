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
 * DQ Artefact FpML Person Role Population Script
 *
 * Populates the dq_person_roles_artefact_tbl with reference data.
 * Dataset: fpml.person_role
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_person_roles() to publish to production.
 */


-- =============================================================================
-- DQ Artefact FpML Person Role
-- =============================================================================

\echo '--- DQ Artefact FpML Person Role ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'fpml.person_role'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.person_role not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores_dq_person_roles_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores_dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Broker',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'The person who arranged with a client to execute the trade.'
    );
    v_count := v_count + 1;
    insert into ores_dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Buyer',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Acquirer of the legal title to the financial instrument.'
    );
    v_count := v_count + 1;
    insert into ores_dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Custodian',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'The operational contact at the custodian.'
    );
    v_count := v_count + 1;
    insert into ores_dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DecisionMaker',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'The party or person with legal responsibility for authorization of the execution of the transaction.'
    );
    v_count := v_count + 1;
    insert into ores_dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ExecutionWithinFirm',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Person within the firm who is responsible for execution of the transaction.'
    );
    v_count := v_count + 1;
    insert into ores_dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'InvestmentDecisionMaker',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Person who is responsible for making the investment decision.'
    );
    v_count := v_count + 1;
    insert into ores_dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LoanCloser',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Individual responsible for managing the closing-related operational servicing of an asset.'
    );
    v_count := v_count + 1;
    insert into ores_dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LoanServicer',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Individual responsible for ongoing operational servicing of the asset. E.g. managing principal draws and repayments, interest and fee payments, etc.'
    );
    v_count := v_count + 1;
    insert into ores_dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Seller',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Seller of the legal title to the financial instrument.'
    );
    v_count := v_count + 1;
    insert into ores_dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Trader',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'The person who executed the trade.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_person_roles_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_person_roles_artefact' as entity, count(*) as count
from ores_dq_person_roles_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores_dq_person_roles_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
