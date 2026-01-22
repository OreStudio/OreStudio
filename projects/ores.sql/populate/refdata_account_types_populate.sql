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
 * Reference Data account_types Population Script
 *
 * Populates the refdata_account_types_tbl with reference data.
 * Source: account_types_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data account_types
-- =============================================================================

\echo '--- Reference Data account_types ---'

insert into ores.refdata_account_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Client',
    0,
    'FPML_ACCOUNT_TYPE',
    'FpML',
    'The account contains trading activity or positions that belong to a client of the firm that opened the account.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_account_types_tbl
    where code = 'Client'
    and coding_scheme_code = 'FPML_ACCOUNT_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_account_types_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'House',
    0,
    'FPML_ACCOUNT_TYPE',
    'FpML',
    'The account contains proprietary trading activity or positions, belonging to the firm that is the owner of the account.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_account_types_tbl
    where code = 'House'
    and coding_scheme_code = 'FPML_ACCOUNT_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'account_types' as entity, count(*) as count
from ores.refdata_account_types_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_account_types_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
