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
 * General Methodology Population Script
 *
 * General-purpose data sourcing methodologies that are not specific to a domain.
 * Domain-specific methodologies are defined in their respective directories.
 * This script is idempotent.
 */

set schema 'metadata';

-- =============================================================================
-- General Methodologies
-- =============================================================================

\echo '--- General Methodologies ---'

select metadata.dq_methodologies_upsert_fn(
    'Synthetic Data Generation',
    'Test data generated programmatically using the ores.synthetic library with seeded random generation',
    'https://github.com/cieslarmichal/faker-cxx',
    'Data Generation Approach:

1. LIBRARY
   Component: ores.synthetic
   Dependencies: faker-cxx library for realistic random data generation

2. GENERATION PROCESS
   - Uses seeded random number generator for reproducibility
   - Generates coherent datasets with proper entity relationships
   - All foreign key references point to valid entities
   - Timestamps and audit fields populated consistently

3. REPRODUCIBILITY
   - Same seed produces identical output
   - Seed value should be tracked by caller for reproducibility
   - Default uses random seed if not specified

4. USAGE
   Code: ores::synthetic::service::catalog_generator_service
   Options: ores::synthetic::domain::generation_options

This methodology is used for all programmatically generated test data.
The specific seed and generation parameters are tracked separately from
the methodology itself.'
);

-- =============================================================================
-- Summary
-- =============================================================================

select 'dq_methodologies' as entity, count(*) as count
from metadata.dq_methodologies_tbl
where valid_to = public.utility_infinity_timestamp_fn();
