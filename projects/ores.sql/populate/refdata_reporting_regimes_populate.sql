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
 * Reference Data reporting_regimes Population Script
 *
 * Populates the refdata_reporting_regimes_tbl with reference data.
 * Source: reporting_regimes_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data reporting_regimes
-- =============================================================================

\echo '--- Reference Data reporting_regimes ---'

insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ASIC',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'Australian Securities and Investments Commission Derivative Transaction Rules (Reporting)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'ASIC'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.Rule.91-507',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'Rule 91-507 Derivatives: Trade Repositories and Derivatives Data. Harmonized rule adopted by Canadian provinces and territories.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'CA.Rule.91-507'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DoddFrankAct',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'Dodd-Frank Act (US)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'DoddFrankAct'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EMIR',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'European Markets Infrastructure Regulation',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'EMIR'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'HKMA',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'Hong Kong Monetary Authority',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'HKMA'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'JFSA',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'Japan Financial Services Authority',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'JFSA'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MAS',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'The Monetary Authority of Singapore',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'MAS'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MiFID',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'Markets in Financial Instruments Directive',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'MiFID'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MiFIDII',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'Markets in Financial Instruments Directive II',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'MiFIDII'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MiFIR',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'Markets in Financial Instruments Regulation',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'MiFIR'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ODRF',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'OTC Derivatives Regulators Forum',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'ODRF'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RussianFederation',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'Russian regulatory reporting',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'RussianFederation'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SFTR',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'Securities Financing Transactions Regulation',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'SFTR'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_reporting_regimes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'UKEMIR',
    0,
    'FPML_REPORTING_REGIME',
    'FpML',
    'United Kingdom European Markets Infrastructure Regulation',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_reporting_regimes_tbl
    where code = 'UKEMIR'
    and coding_scheme_code = 'FPML_REPORTING_REGIME'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'reporting_regimes' as entity, count(*) as count
from ores.refdata_reporting_regimes_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_reporting_regimes_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
