/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
 * Data Quality Origin Dimension Population Script
 *
 * Seeds the database with origin dimension values for data quality classification.
 * This script is idempotent.
 *
 * Origins:
 * - Primary: Raw data ingested directly from the origin without transformations
 * - Derived: Data transformed, aggregated, or calculated via code
 */

-- =============================================================================
-- Migration: Rename Source to Primary
-- =============================================================================

-- Update existing 'Source' records to 'Primary'
update ores_dq_origin_dimensions_tbl
set code = 'Primary', name = 'Primary Data'
where code = 'Source';

-- Update any datasets referencing 'Source' to use 'Primary'
update ores_dq_datasets_tbl
set origin_code = 'Primary'
where origin_code = 'Source';

-- =============================================================================
-- Data Quality Origin Dimensions
-- =============================================================================

\echo '--- Data Quality Origin Dimensions ---'

select ores_dq_origin_dimensions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'Primary',
    'Primary Data',
    'Raw data ingested directly from the origin without transformations.'
);

select ores_dq_origin_dimensions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'Derived',
    'Derived Data',
    'Data transformed, aggregated, or calculated via code.'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Origin Dimensions' as entity, count(*) as count
from ores_dq_origin_dimensions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
order by entity;