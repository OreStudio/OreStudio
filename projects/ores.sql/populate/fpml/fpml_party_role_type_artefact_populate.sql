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
 * DQ Artefact FpML Party Role Type Population Script
 *
 * Populates the dq_party_roles_artefact_tbl with reference data.
 * Dataset: fpml.party_role_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_party_roles() to publish to production.
 */

set schema 'metadata';

-- =============================================================================
-- DQ Artefact FpML Party Role Type
-- =============================================================================

\echo '--- DQ Artefact FpML Party Role Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from metadata.dq_datasets_tbl
    where code = 'fpml.party_role_type'
    and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.party_role_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from metadata.dq_party_roles_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into metadata.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AllPositions',
        1,
        'FPML_PARTY_ROLE_TYPE',
        'FpML',
        'All Positions.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SomePositions',
        1,
        'FPML_PARTY_ROLE_TYPE',
        'FpML',
        'Some Positions.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_party_roles_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_party_roles_artefact' as entity, count(*) as count
from metadata.dq_party_roles_artefact_tbl;

select coding_scheme_code, count(*) as count
from metadata.dq_party_roles_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
