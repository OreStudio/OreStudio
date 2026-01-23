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
 * DQ Artefact FpML Non Iso Currency Population Script
 *
 * Populates the dq_non_iso_currencies_artefact_tbl with reference data.
 * Dataset: fpml.non_iso_currency
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_non_iso_currencies() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Non Iso Currency
-- =============================================================================

\echo '--- DQ Artefact FpML Non Iso Currency ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.non_iso_currency'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.non_iso_currency not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_non_iso_currencies_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_non_iso_currencies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CNH',
        1,
        'FPML_NON_ISO_CURRENCY',
        'FpML',
        'Offshore Chinese Yuan traded in Hong Kong.'
    );
    v_count := v_count + 1;
    insert into ores.dq_non_iso_currencies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CNT',
        1,
        'FPML_NON_ISO_CURRENCY',
        'FpML',
        'Offshore Chinese Yuan traded in Taiwan.'
    );
    v_count := v_count + 1;
    insert into ores.dq_non_iso_currencies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GGP',
        1,
        'FPML_NON_ISO_CURRENCY',
        'FpML',
        'Guernsey Pound.'
    );
    v_count := v_count + 1;
    insert into ores.dq_non_iso_currencies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IMP',
        1,
        'FPML_NON_ISO_CURRENCY',
        'FpML',
        'Isle of Man Pound.'
    );
    v_count := v_count + 1;
    insert into ores.dq_non_iso_currencies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JEP',
        1,
        'FPML_NON_ISO_CURRENCY',
        'FpML',
        'Jersey Pound.'
    );
    v_count := v_count + 1;
    insert into ores.dq_non_iso_currencies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KID',
        1,
        'FPML_NON_ISO_CURRENCY',
        'FpML',
        'Kiribati Dollar.'
    );
    v_count := v_count + 1;
    insert into ores.dq_non_iso_currencies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MCF',
        1,
        'FPML_NON_ISO_CURRENCY',
        'FpML',
        'Monegasque franc. Historical currency code.'
    );
    v_count := v_count + 1;
    insert into ores.dq_non_iso_currencies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SML',
        1,
        'FPML_NON_ISO_CURRENCY',
        'FpML',
        'Sammarinese lira. Hhistorical currency code.'
    );
    v_count := v_count + 1;
    insert into ores.dq_non_iso_currencies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TVD',
        1,
        'FPML_NON_ISO_CURRENCY',
        'FpML',
        'Tuvalu Dollar.'
    );
    v_count := v_count + 1;
    insert into ores.dq_non_iso_currencies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VAL',
        1,
        'FPML_NON_ISO_CURRENCY',
        'FpML',
        'Vatican lira. Historical currency code.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_non_iso_currencies_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_non_iso_currencies_artefact' as entity, count(*) as count
from ores.dq_non_iso_currencies_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_non_iso_currencies_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
