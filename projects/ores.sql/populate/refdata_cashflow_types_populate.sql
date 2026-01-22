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
 * Reference Data cashflow_types Population Script
 *
 * Populates the refdata_cashflow_types_tbl with reference data.
 * Source: cashflow_types_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data cashflow_types
-- =============================================================================

\echo '--- Reference Data cashflow_types ---'

insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AmendmentFee',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow associated with an amendment lifecycle event.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'AmendmentFee'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AssignmentFee',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow resulting from the assignment of a contract to a new counterparty.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'AssignmentFee'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Coupon',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow corresponding to the periodic accrued interests.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'Coupon'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CreditEvent',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cashflow resulting from a credit event.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'CreditEvent'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DividendReturn',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow corresponding to the synthetic dividend of an equity underlyer asset traded through a derivative instrument.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'DividendReturn'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ExerciseFee',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow associated with an exercise lifecycle event.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'ExerciseFee'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Fee',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A generic term for describing a non-scheduled cashflow that can be associated either with the initial contract, with some later corrections to it (e.g. a correction to the day count fraction that has a cashflow impact) or with some lifecycle events. Fees that are specifically associated with termination and partial termination, increase, amendment, and exercise events are qualified accordingly.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'Fee'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'IncreaseFee',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow associated with an increase lifecycle event.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'IncreaseFee'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'InterestReturn',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow corresponding to the return of the interest rate portion of a derivative instrument that has different types of underlying assets, such as a total return swap.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'InterestReturn'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PartialTerminationFee',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow associated with a partial termination lifecycle event.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'PartialTerminationFee'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Premium',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'The premium associated with an OTC contract such as an option or a cap/floor.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'Premium'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PriceReturn',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow corresponding to the return of the price portion of a derivative instrument that has different types of underlying assets, such as a total return swap.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'PriceReturn'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PrincipalExchange',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow which amount typically corresponds to the notional of the contract and that is exchanged between the parties on trade inception and reverted back when the contract is terminated.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'PrincipalExchange'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_cashflow_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TerminationFee',
    0,
    'FPML_CASHFLOW_TYPE',
    'FpML',
    'A cash flow associated with a termination lifecycle event.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_cashflow_types_tbl
    where code = 'TerminationFee'
    and coding_scheme_code = 'FPML_CASHFLOW_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'cashflow_types' as entity, count(*) as count
from ores.refdata_cashflow_types_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_cashflow_types_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
