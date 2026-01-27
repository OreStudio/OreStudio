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

set schema 'metadata';

-- =============================================================================
-- Data Quality Slovaris Datasets
-- =============================================================================

\echo '--- Data Quality Slovaris Datasets ---'

select public.upsert_dq_datasets(
    'slovaris.country_flags',
    'Slovaris',
    'Country Flags',
    'Reference Data',
    'NONE',
    'Primary',
    'Synthetic',
    'Raw',
    'OreStudio Code Generation Methodology',
    'Solvaris Country Flag Images',
    'SVG flag images for each Solvaris country.',
    'SOLVARIS',
    'Visual assets for solvaris countries',
    current_date,
    ' CC BY 4.0',
    'images'
);
select public.upsert_dq_datasets(
    'slovaris.countries',
    'Slovaris',
    'Countries',
    'Reference Data',
    'NONE',
    'Primary',
    'Synthetic',
    'Raw',
    'OreStudio Code Generation Methodology',
    'Solvaris Countries',
    'Solvaris country codes and official names.',
    'SOLVARIS',
    'Reference data for solvaris country codes',
    current_date,
    ' CC BY 4.0',
    'countries'
);
select public.upsert_dq_datasets(
    'slovaris.currencies',
    'Slovaris',
    'Currencies',
    'Reference Data',
    'NONE',
    'Primary',
    'Synthetic',
    'Raw',
    'OreStudio Code Generation Methodology',
    'Solvaris Currencies',
    'Solvaris alphabetic and numeric currency codes.',
    'SOLVARIS',
    'Reference data for solvaris country codes',
    current_date,
    ' CC BY 4.0',
    'currencies'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Total Datasets' as entity, count(*) as count
from metadata.dq_datasets_tbl where valid_to = public.utility_infinity_timestamp_fn()
order by entity;
