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
 * Test Data Master Include File
 *
 * Includes all test data SQL files in the correct dependency order.
 * Business units must come before portfolios (owner_unit_id references),
 * and portfolios must come before books (parent_portfolio_id references).
 */

-- =============================================================================
-- Test Data Catalog (must come first)
-- =============================================================================

\echo '--- Test Data Catalog ---'
\ir testdata_catalog_populate.sql

-- =============================================================================
-- Test Data Methodology
-- =============================================================================

\echo '--- Test Data Methodology ---'
\ir testdata_methodology_populate.sql

-- =============================================================================
-- Test Data Datasets
-- =============================================================================

\echo '--- Test Data Datasets ---'
\ir testdata_dataset_populate.sql

-- =============================================================================
-- Test Data Business Units Artefacts
-- =============================================================================

\echo '--- Test Data Business Units Artefacts ---'
\ir testdata_business_units_artefact_populate.sql

-- =============================================================================
-- Test Data Portfolios Artefacts
-- =============================================================================

\echo '--- Test Data Portfolios Artefacts ---'
\ir testdata_portfolios_artefact_populate.sql

-- =============================================================================
-- Test Data Books Artefacts
-- =============================================================================

\echo '--- Test Data Books Artefacts ---'
\ir testdata_books_artefact_populate.sql
