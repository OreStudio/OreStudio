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
 * DQ Artefact FpML Account Type Population Script
 *
 * Populates the dq_account_types_artefact_tbl with reference data.
 * Dataset: fpml.account_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_account_types() to publish to production.
 */

set schema 'metadata';

-- =============================================================================
-- DQ Artefact FpML Account Type
-- =============================================================================

\echo '--- DQ Artefact FpML Account Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from metadata.dq_datasets_tbl
    where code = 'fpml.account_type'
    and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.account_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from metadata.dq_account_types_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into metadata.dq_account_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AggregateClient',
        1,
        'FPML_ACCOUNT_TYPE',
        'FpML',
        'Aggregate client account, as defined under ESMA MiFIR.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_account_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Client',
        1,
        'FPML_ACCOUNT_TYPE',
        'FpML',
        'The account contains trading activity or positions that belong to a client of the firm that opened the account.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_account_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'House',
        1,
        'FPML_ACCOUNT_TYPE',
        'FpML',
        'The account contains proprietary trading activity or positions, belonging to the firm that is the owner of the account.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_account_types_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_account_types_artefact' as entity, count(*) as count
from metadata.dq_account_types_artefact_tbl;

select coding_scheme_code, count(*) as count
from metadata.dq_account_types_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
