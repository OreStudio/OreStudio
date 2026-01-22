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
 * Reference Data party_relationships Population Script
 *
 * Populates the refdata_party_relationships_tbl with reference data.
 * Source: party_relationships_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data party_relationships
-- =============================================================================

\echo '--- Reference Data party_relationships ---'

insert into ores.refdata_party_relationships_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Intragroup',
    0,
    'FPML_HKMA_REWRITE_PARTY_RELATIONSHIP_TYPE',
    'FpML',
    'Intragroup as defined by Hong Kong Monetary Authority (HKMA) Rewrite field 189 - Intragroup.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_relationships_tbl
    where code = 'Intragroup'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_PARTY_RELATIONSHIP_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_relationships_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NotIntragroup',
    0,
    'FPML_HKMA_REWRITE_PARTY_RELATIONSHIP_TYPE',
    'FpML',
    'Not intragroup as defined by Hong Kong Monetary Authority (HKMA) Rewrite field 189 - Intragroup.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_relationships_tbl
    where code = 'NotIntragroup'
    and coding_scheme_code = 'FPML_HKMA_REWRITE_PARTY_RELATIONSHIP_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_relationships_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Affiliated',
    0,
    'FPML_PARTY_RELATIONSHIP_TYPE',
    'FpML',
    'Indicates whether the transaction is between two affiliated entities. It is referred to as Inter-affiliate under the Canadian CSA reporting regime.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_relationships_tbl
    where code = 'Affiliated'
    and coding_scheme_code = 'FPML_PARTY_RELATIONSHIP_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_relationships_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Inter-Dealer',
    0,
    'FPML_PARTY_RELATIONSHIP_TYPE',
    'FpML',
    'Indicates the transaction is between two dealers.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_relationships_tbl
    where code = 'Inter-Dealer'
    and coding_scheme_code = 'FPML_PARTY_RELATIONSHIP_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_relationships_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Intragroup',
    0,
    'FPML_PARTY_RELATIONSHIP_TYPE',
    'FpML',
    'Indicates whether the contract was concluded as an intra-group transaction, defined in Article 3, 4(2), 11(6) to 11(10) of EMIR.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_relationships_tbl
    where code = 'Intragroup'
    and coding_scheme_code = 'FPML_PARTY_RELATIONSHIP_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'party_relationships' as entity, count(*) as count
from ores.refdata_party_relationships_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_party_relationships_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
