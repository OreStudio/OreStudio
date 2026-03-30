/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Data Quality Framework Population Script
 *
 * Includes DQ framework items that are NOT part of the foundation layer.
 * Foundation layer items (change control, data domains, subject areas,
 * coding scheme authority types, coding schemes) are populated separately
 * via populate_foundation.sql.
 *
 * This script populates:
 * - Badge system (severities, code domains, definitions, mappings)
 * - Dimensions (origin, nature, treatment)
 * - Methodologies
 * - Artefact types
 * - Dataset bundles and bundle members
 */

-- =============================================================================
-- Badge System (severities and code domains before definitions and mappings)
-- =============================================================================

\echo '--- Badge System ---'
\ir dq_badge_system_populate.sql

-- =============================================================================
-- Dimensions (Origin, Nature, Treatment)
-- =============================================================================

\echo '--- Dimensions ---'
\ir dq_origin_dimension_populate.sql
\ir dq_nature_dimension_populate.sql
\ir dq_treatment_dimension_populate.sql

-- =============================================================================
-- General Methodologies
-- =============================================================================

\echo '--- General Methodologies ---'
\ir dq_methodology_populate.sql

-- =============================================================================
-- Artefact Types (must precede datasets for FK validation)
-- =============================================================================

\echo '--- Artefact Types ---'
\ir dq_artefact_types_populate.sql

-- =============================================================================
-- Dataset Bundles (must follow artefact types)
-- =============================================================================

\echo '--- Dataset Bundles ---'
\ir dq_dataset_bundle_populate.sql

-- =============================================================================
-- Dataset Bundle Members (must follow bundles and datasets)
-- =============================================================================

\echo '--- Dataset Bundle Members ---'
\ir dq_dataset_bundle_member_populate.sql
