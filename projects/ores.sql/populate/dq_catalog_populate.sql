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
 * Data Quality Catalog Population Script
 *
 * Seeds the database with catalog values for grouping related datasets.
 * This script is idempotent.
 *
 * NOTE: Most catalogs are now defined in their respective domain directories:
 * - crypto/crypto_catalog_populate.sql: Cryptocurrency catalog
 * - flags/flags_catalog_populate.sql: Visual Assets catalog
 * - fpml/fpml_catalog_populate.sql: FpML Standards catalog
 * - ip2country/ip2country_catalog_populate.sql: IP Geolocation catalog
 * - solvaris/solvaris_catalog_populate.sql: Slovaris catalog
 *
 * This file only contains the ISO Standards catalog for core country/currency data.
 */

set schema 'ores';

-- =============================================================================
-- Data Quality Catalogs
-- =============================================================================

\echo '--- Data Quality Catalogs ---'

select ores.upsert_dq_catalogs(
    'ISO Standards',
    'International Organization for Standardization (ISO) reference data including ISO 3166 country codes and ISO 4217 currency codes.',
    'Reference Data Team'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Catalogs' as entity, count(*) as count
from ores.dq_catalogs_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;
