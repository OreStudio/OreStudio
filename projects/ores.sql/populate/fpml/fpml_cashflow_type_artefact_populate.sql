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
 * DQ Artefact FpML Cashflow Type Population Script
 *
 * Populates the dq_cashflow_types_artefact_tbl with reference data.
 * Dataset: fpml.cashflow_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_cashflow_types() to publish to production.
 */


-- =============================================================================
-- DQ Artefact FpML Cashflow Type
-- =============================================================================

\echo '--- DQ Artefact FpML Cashflow Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'fpml.cashflow_type'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.cashflow_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores_dq_cashflow_types_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AmendmentFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow associated with an amendment lifecycle event.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AssignmentFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow resulting from the assignment of a contract to a new counterparty.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Coupon',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow corresponding to the periodic accrued interests.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CreditEvent',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cashflow resulting from a credit event.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DividendReturn',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow corresponding to the synthetic dividend of an equity underlyer asset traded through a derivative instrument.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ExerciseFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow associated with an exercise lifecycle event.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Fee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A generic term for describing a non-scheduled cashflow that can be associated either with the initial contract, with some later corrections to it (e.g. a correction to the day count fraction that has a cashflow impact) or with some lifecycle events. Fees that are specifically associated with termination and partial termination, increase, amendment, and exercise events are qualified accordingly.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IncreaseFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow associated with an increase lifecycle event.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'InterestReturn',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow corresponding to the return of the interest rate portion of a derivative instrument that has different types of underlying assets, such as a total return swap.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PartialTerminationFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow associated with a partial termination lifecycle event.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Premium',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'The premium associated with an OTC contract such as an option or a cap/floor.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PriceReturn',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow corresponding to the return of the price portion of a derivative instrument that has different types of underlying assets, such as a total return swap.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PrincipalExchange',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow which amount typically corresponds to the notional of the contract and that is exchanged between the parties on trade inception and reverted back when the contract is terminated.'
    );
    v_count := v_count + 1;
    insert into ores_dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TerminationFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow associated with a termination lifecycle event.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_cashflow_types_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_cashflow_types_artefact' as entity, count(*) as count
from ores_dq_cashflow_types_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores_dq_cashflow_types_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
