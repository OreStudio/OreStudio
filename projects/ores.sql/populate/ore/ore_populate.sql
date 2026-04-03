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

-- =============================================================================
-- ORE Reference Data Master Population File
--
-- Seeds ORE-specific reference data: catalog, coding scheme dataset, and asset
-- class artefacts. After staging, publishes ORE (and FpML) asset classes to
-- ores_refdata_asset_classes_tbl so that market data series validation works
-- from initial database setup.
-- =============================================================================

\echo '--- ORE Catalog ---'
\ir ore_catalog_populate.sql

\echo ''
\echo '--- ORE Asset Class Dataset ---'
\ir ore_asset_class_dataset_populate.sql

\echo ''
\echo '--- ORE Asset Class Artefacts ---'
\ir ore_asset_class_artefact_populate.sql

-- =============================================================================
-- Publish FpML asset classes to production
-- (ensures production table is populated for market data validation)
-- =============================================================================

\echo ''
\echo '--- Publishing FpML Asset Classes ---'
select * from ores_dq_asset_classes_publish_fn(
    (select id from ores_dq_datasets_tbl
     where code = 'fpml.asset_class'
     and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_iam_system_tenant_id_fn(),
    'upsert'
);

-- =============================================================================
-- Publish ORE asset classes to production
-- =============================================================================

\echo ''
\echo '--- Publishing ORE Asset Classes ---'
select * from ores_dq_asset_classes_publish_fn(
    (select id from ores_dq_datasets_tbl
     where code = 'ore.asset_class'
     and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_iam_system_tenant_id_fn(),
    'upsert'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- ORE Population Summary ---'

select 'DQ: ORE Asset Class Artefacts' as entity, count(*) as count
from ores_dq_asset_classes_artefact_tbl
where dataset_id = (
    select id from ores_dq_datasets_tbl
    where code = 'ore.asset_class'
    and valid_to = ores_utility_infinity_timestamp_fn()
)
union all
select 'Refdata: Asset Classes (all schemes)', count(*)
from ores_refdata_asset_classes_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Refdata: ORE Asset Classes', count(*)
from ores_refdata_asset_classes_tbl
where coding_scheme_code = 'ORE_ASSET_CLASS'
and valid_to = ores_utility_infinity_timestamp_fn()
order by entity;
