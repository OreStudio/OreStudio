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
 * Dataset Bundle Population Script
 *
 * Seeds the database with dataset bundle definitions.
 * Bundles are named collections of datasets designed to work together.
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- Dataset Bundles
-- =============================================================================

\echo '--- Dataset Bundles ---'

select ores.upsert_dq_dataset_bundle(
    'solvaris',
    'Solvaris',
    'Synthetic reference data for development and testing - an isolated fantasy world with its own countries, currencies, and reference data.'
);

select ores.upsert_dq_dataset_bundle(
    'base',
    'Base System',
    'Industry-standard reference data (ISO + FpML) for production use. Includes country codes, currency codes, and financial market standards.'
);

select ores.upsert_dq_dataset_bundle(
    'crypto',
    'Crypto',
    'Base System plus cryptocurrency reference data. Extends the production dataset with cryptocurrency symbols and icons.'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Dataset Bundles' as entity, count(*) as count
from ores.dq_dataset_bundles_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;
