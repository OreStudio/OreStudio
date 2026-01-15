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
 * Reference Data Population Script
 *
 * Seeds the database with optional reference data:
 * - Country flags (images and tags)
 * - Currencies (ISO 4217)
 * - Currency-image associations
 *
 * This data is NOT required for the application to function, but provides
 * useful reference data for users. Run this after creating a database
 * instance if you want pre-populated reference data.
 *
 * Usage:
 *   psql -U ores -d your_database -f populate/reference_data.sql
 */

-- Suppress noisy output during population
\timing off
\pset tuples_only on

\echo '=== Starting Reference Data Population ==='
\echo ''

-- Flag images and tags
\echo '--- Flag Data ---'
\ir assets_load_flags.sql
\ir assets_flags_populate.sql

-- Currencies
\echo ''
\echo '--- Currency Data ---'
\ir refdata_currencies_populate.sql
\ir assets_currency_images_populate.sql

\echo ''
\echo '=== Reference Data Population Complete ==='

-- Summary - restore normal output format
\pset tuples_only off

\echo ''
\echo '--- Summary ---'

select 'Currencies' as entity, count(*) as count
from ores.refdata_currencies_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Images', count(*)
from ores.assets_images_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Tags', count(*)
from ores.assets_tags_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;
