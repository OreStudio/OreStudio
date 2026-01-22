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
 * Reference Data regulatory_corporate_sectors Population Script
 *
 * Populates the refdata_regulatory_corporate_sectors_tbl with reference data.
 * Source: regulatory_corporate_sectors_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data regulatory_corporate_sectors
-- =============================================================================

\echo '--- Reference Data regulatory_corporate_sectors ---'

insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AIFD',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'Alternative Investment Fund.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'AIFD'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ASSU',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'Assurance Undertaking.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'ASSU'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CCPS',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'Central Counterparty.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'CCPS'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CDTI',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'Credit Institution.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'CDTI'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CSDS',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'Central Securities Depository.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'CSDS'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'INUN',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'Insurance Undertaking.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'INUN'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'INVF',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'Investment Firm.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'INVF'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ORPI',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'Occupational Retirement Provision Institution.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'ORPI'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'OTHR',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'Other.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'OTHR'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'REIN',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'Reinsurance Undertaking.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'REIN'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_regulatory_corporate_sectors_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'UCIT',
    0,
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'HKMA',
    'UCITS Management Company.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_regulatory_corporate_sectors_tbl
    where code = 'UCIT'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'regulatory_corporate_sectors' as entity, count(*) as count
from ores.refdata_regulatory_corporate_sectors_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_regulatory_corporate_sectors_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
