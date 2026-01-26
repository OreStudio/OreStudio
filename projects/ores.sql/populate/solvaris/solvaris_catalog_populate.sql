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
-- Data Quality Slovaris Catalogs
-- =============================================================================

\echo '--- Data Quality Slovaris Catalogs ---'

select public.upsert_dq_catalogs(
    'Slovaris',
    'Imaginary world to test all system functions.',
    'Testing Team'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Total Catalogs' as entity, count(*) as count
from metadata.dq_catalogs_tbl where valid_to = public.utility_infinity_timestamp_fn()
order by entity;
