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
 * - Dimensions (origin, nature, treatment)
 * - Methodologies
 * - Artefact types
 */

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
