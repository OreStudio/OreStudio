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
-- Data Quality Slovaris Tags
-- =============================================================================

\echo '--- Data Quality Slovaris Tags ---'

select metadata.dq_tags_upsert_fn(
    'Solvaris Countries',
    'Countries',
    'Reference Data',
    'country',
    'Country reference data'
);
select metadata.dq_tags_upsert_fn(
    'Solvaris Currencies',
    'Currencies',
    'Reference Data',
    'currency',
    'Currency reference data'
);
select metadata.dq_tags_upsert_fn(
    'Solvaris Country Flag Images',
    'Country Flags',
    'Reference Data',
    'flag',
    'Country and region flag images'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_datasets' as entity, count(*) as count
from metadata.dq_datasets_tbl
where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'dq_tags_artefact', count(*)
from metadata.dq_tags_artefact_tbl;
