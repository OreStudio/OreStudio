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
 * Reference Data asset_classes Population Script
 *
 * Populates the refdata_asset_classes_tbl with reference data.
 * Source: asset_classes_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data asset_classes
-- =============================================================================

\echo '--- Reference Data asset_classes ---'

insert into ores.refdata_asset_classes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Commodity',
    0,
    'FPML_ASSET_CLASS',
    'FpML',
    'Commodity.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_classes_tbl
    where code = 'Commodity'
    and coding_scheme_code = 'FPML_ASSET_CLASS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_classes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Credit',
    0,
    'FPML_ASSET_CLASS',
    'FpML',
    'Credit.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_classes_tbl
    where code = 'Credit'
    and coding_scheme_code = 'FPML_ASSET_CLASS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_classes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Equity',
    0,
    'FPML_ASSET_CLASS',
    'FpML',
    'Equity.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_classes_tbl
    where code = 'Equity'
    and coding_scheme_code = 'FPML_ASSET_CLASS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_classes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ForeignExchange',
    0,
    'FPML_ASSET_CLASS',
    'FpML',
    'ForeignExchange.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_classes_tbl
    where code = 'ForeignExchange'
    and coding_scheme_code = 'FPML_ASSET_CLASS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_classes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'InterestRate',
    0,
    'FPML_ASSET_CLASS',
    'FpML',
    'InterestRate.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_classes_tbl
    where code = 'InterestRate'
    and coding_scheme_code = 'FPML_ASSET_CLASS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_classes_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SecuritiesFinancing',
    0,
    'FPML_ASSET_CLASS',
    'FpML',
    'SecuritiesFinancing.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_classes_tbl
    where code = 'SecuritiesFinancing'
    and coding_scheme_code = 'FPML_ASSET_CLASS'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'asset_classes' as entity, count(*) as count
from ores.refdata_asset_classes_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_asset_classes_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
