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
 * ISO Standards Dataset Dependency Population Script
 *
 * Seeds the database with dataset dependencies for ISO Standards datasets.
 * Dependencies:
 * - iso.countries depends on assets.country_flags for flag display
 * - iso.currencies depends on assets.country_flags for flag display
 *
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- ISO Standards Dataset Dependencies
-- =============================================================================

\echo '--- ISO Standards Dataset Dependencies ---'

-- ISO countries uses flag images from Visual Assets
select ores.upsert_dq_dataset_dependency(
    'iso.countries',
    'assets.country_flags',
    'visual_assets'
);

-- ISO currencies uses flag images from Visual Assets
select ores.upsert_dq_dataset_dependency(
    'iso.currencies',
    'assets.country_flags',
    'visual_assets'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select dataset_code, dependency_code, role
from ores.dq_dataset_dependencies_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
order by dataset_code, dependency_code;
