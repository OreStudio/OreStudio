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
 * Data Quality Nature Dimension Population Script
 *
 * Seeds the database with nature dimension values for data quality classification.
 * This script is idempotent.
 *
 * Natures:
 * - Actual: Real-world data (replaces "Real")
 * - Synthetic: Artificially generated data for testing/modeling
 * - Mock: Static, hand-written data for unit tests
 */

-- =============================================================================
-- Data Quality Nature Dimensions
-- =============================================================================

\echo '--- Data Quality Nature Dimensions ---'

select ores_dq_nature_dimensions_upsert_fn(
    'Actual',
    'Actual Data',
    'Real-world data (replaces "Real").'
);

select ores_dq_nature_dimensions_upsert_fn(
    'Synthetic',
    'Synthetic Data',
    'Artificially generated data for testing/modeling.'
);

select ores_dq_nature_dimensions_upsert_fn(
    'Mock',
    'Mock Data',
    'Static, hand-written data for unit tests.'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Nature Dimensions' as entity, count(*) as count
from ores_dq_nature_dimensions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
order by entity;