/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * ISO Standards Reference Data Master Include File
 *
 * Includes all ISO Standards SQL files in the correct dependency order.
 */

-- =============================================================================
-- ISO Standards Catalog (must come first)
-- =============================================================================

\echo '--- ISO Standards Catalog ---'
\ir iso_catalog_populate.sql

-- =============================================================================
-- ISO Standards Methodology
-- =============================================================================

\echo '--- ISO Standards Methodology ---'
\ir iso_methodology_populate.sql

-- =============================================================================
-- ISO Standards Datasets
-- =============================================================================

\echo '--- ISO Standards Datasets ---'
\ir iso_dataset_populate.sql

-- =============================================================================
-- ISO Standards Dataset Tags
-- =============================================================================

\echo '--- ISO Standards Dataset Tags ---'
\ir iso_dataset_tag_populate.sql

-- =============================================================================
-- ISO Standards Dataset Dependencies
-- =============================================================================

\echo '--- ISO Standards Dataset Dependencies ---'
\ir iso_dataset_dependency_populate.sql

-- =============================================================================
-- ISO Countries Artefacts
-- =============================================================================

\echo '--- ISO Countries Artefacts ---'
\ir iso_countries_artefact_populate.sql

-- =============================================================================
-- ISO Currencies Artefacts
-- =============================================================================

\echo '--- ISO Currencies Artefacts ---'
\ir iso_currencies_artefact_populate.sql
