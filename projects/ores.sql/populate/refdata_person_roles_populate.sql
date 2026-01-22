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
 * Reference Data person_roles Population Script
 *
 * Populates the refdata_person_roles_tbl with reference data.
 * Source: person_roles_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data person_roles
-- =============================================================================

\echo '--- Reference Data person_roles ---'

insert into ores.refdata_person_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Broker',
    0,
    'FPML_PERSON_ROLE',
    'FpML',
    'The person who arranged with a client to execute the trade.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_person_roles_tbl
    where code = 'Broker'
    and coding_scheme_code = 'FPML_PERSON_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_person_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Buyer',
    0,
    'FPML_PERSON_ROLE',
    'FpML',
    'Acquirer of the legal title to the financial instrument.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_person_roles_tbl
    where code = 'Buyer'
    and coding_scheme_code = 'FPML_PERSON_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_person_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Custodian',
    0,
    'FPML_PERSON_ROLE',
    'FpML',
    'The operational contact at the custodian.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_person_roles_tbl
    where code = 'Custodian'
    and coding_scheme_code = 'FPML_PERSON_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_person_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DecisionMaker',
    0,
    'FPML_PERSON_ROLE',
    'FpML',
    'The party or person with legal responsibility for authorization of the execution of the transaction.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_person_roles_tbl
    where code = 'DecisionMaker'
    and coding_scheme_code = 'FPML_PERSON_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_person_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ExecutionWithinFirm',
    0,
    'FPML_PERSON_ROLE',
    'FpML',
    'Person within the firm who is responsible for execution of the transaction.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_person_roles_tbl
    where code = 'ExecutionWithinFirm'
    and coding_scheme_code = 'FPML_PERSON_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_person_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'InvestmentDecisionMaker',
    0,
    'FPML_PERSON_ROLE',
    'FpML',
    'Person who is responsible for making the investment decision.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_person_roles_tbl
    where code = 'InvestmentDecisionMaker'
    and coding_scheme_code = 'FPML_PERSON_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_person_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LoanCloser',
    0,
    'FPML_PERSON_ROLE',
    'FpML',
    'Individual responsible for managing the closing-related operational servicing of an asset.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_person_roles_tbl
    where code = 'LoanCloser'
    and coding_scheme_code = 'FPML_PERSON_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_person_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LoanServicer',
    0,
    'FPML_PERSON_ROLE',
    'FpML',
    'Individual responsible for ongoing operational servicing of the asset. E.g. managing principal draws and repayments, interest and fee payments, etc.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_person_roles_tbl
    where code = 'LoanServicer'
    and coding_scheme_code = 'FPML_PERSON_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_person_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Seller',
    0,
    'FPML_PERSON_ROLE',
    'FpML',
    'Seller of the legal title to the financial instrument.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_person_roles_tbl
    where code = 'Seller'
    and coding_scheme_code = 'FPML_PERSON_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_person_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Trader',
    0,
    'FPML_PERSON_ROLE',
    'FpML',
    'The person who executed the trade.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_person_roles_tbl
    where code = 'Trader'
    and coding_scheme_code = 'FPML_PERSON_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'person_roles' as entity, count(*) as count
from ores.refdata_person_roles_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_person_roles_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
