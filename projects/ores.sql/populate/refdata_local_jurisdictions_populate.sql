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
 * Reference Data local_jurisdictions Population Script
 *
 * Populates the refdata_local_jurisdictions_tbl with reference data.
 * Source: local_jurisdictions_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data local_jurisdictions
-- =============================================================================

\echo '--- Reference Data local_jurisdictions ---'

insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Afghanistan',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Afghan Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Afghanistan'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Applicable',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Follows Local Jurisdiction as per MCA to this Transaction.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Applicable'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Australia',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Australian Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Australia'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'China',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Chinese Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'China'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'HongKong',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Hong Kong Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'HongKong'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'India',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Indian Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'India'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Indonesia',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Indonesian Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Indonesia'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Japan',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Japanese Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Japan'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Korea',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Korean Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Korea'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Malaysia',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Malaysian Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Malaysia'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NewZealand',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'New Zealand Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'NewZealand'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NotApplicable',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'No Local Jurisdiction applies to this Transaction.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'NotApplicable'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Pakistan',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Pakistani Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Pakistan'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Philippines',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Philippine Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Philippines'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Singapore',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Singaporean Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Singapore'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Taiwan',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Taiwanese Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Taiwan'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Thailand',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Thai Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Thailand'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_local_jurisdictions_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Vietnam',
    0,
    'FPML_LOCAL_JURISDICTION',
    'FpML',
    'Vietnamese Local Jurisdiction applies.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_local_jurisdictions_tbl
    where code = 'Vietnam'
    and coding_scheme_code = 'FPML_LOCAL_JURISDICTION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'local_jurisdictions' as entity, count(*) as count
from ores.refdata_local_jurisdictions_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_local_jurisdictions_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
