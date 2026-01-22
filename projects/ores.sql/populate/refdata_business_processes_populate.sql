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
 * Reference Data business_processes Population Script
 *
 * Populates the refdata_business_processes_tbl with reference data.
 * Source: business_processes_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data business_processes
-- =============================================================================

\echo '--- Reference Data business_processes ---'

insert into ores.refdata_business_processes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Allocation',
    0,
    'FPML_BUSINESS_PROCESS',
    'FpML',
    'Process for splitting a trade across accounts.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_processes_tbl
    where code = 'Allocation'
    and coding_scheme_code = 'FPML_BUSINESS_PROCESS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_processes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Clearing',
    0,
    'FPML_BUSINESS_PROCESS',
    'FpML',
    'Process for novating a trade to a central counterparty (with margining) for credit risk mitigation.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_processes_tbl
    where code = 'Clearing'
    and coding_scheme_code = 'FPML_BUSINESS_PROCESS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_processes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Confirmation',
    0,
    'FPML_BUSINESS_PROCESS',
    'FpML',
    'Process for verifying the terms of a trade.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_processes_tbl
    where code = 'Confirmation'
    and coding_scheme_code = 'FPML_BUSINESS_PROCESS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_processes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Execution',
    0,
    'FPML_BUSINESS_PROCESS',
    'FpML',
    'Process for executing a trade.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_processes_tbl
    where code = 'Execution'
    and coding_scheme_code = 'FPML_BUSINESS_PROCESS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_processes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Reconciliation',
    0,
    'FPML_BUSINESS_PROCESS',
    'FpML',
    'Process for comparing representations of a trade or portfolio for the purpose of identifying and resolving discrepancies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_processes_tbl
    where code = 'Reconciliation'
    and coding_scheme_code = 'FPML_BUSINESS_PROCESS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_processes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Settlement',
    0,
    'FPML_BUSINESS_PROCESS',
    'FpML',
    'Process for calculating payment amounts and performing payments as required by the terms of a transaction.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_processes_tbl
    where code = 'Settlement'
    and coding_scheme_code = 'FPML_BUSINESS_PROCESS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'business_processes' as entity, count(*) as count
from ores.refdata_business_processes_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_business_processes_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
