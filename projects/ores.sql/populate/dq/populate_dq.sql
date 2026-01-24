/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Data Quality Framework Master Include File
 *
 * Includes all DQ framework SQL files in the correct dependency order.
 * These are the core DQ tables that other domain-specific files depend on.
 */

-- =============================================================================
-- Change Control (must be populated before entities that use reasons)
-- =============================================================================

\echo '--- Change Control ---'
\ir dq_change_reasons_populate.sql

-- =============================================================================
-- Data Domains
-- =============================================================================

\echo '--- Data Domains ---'
\ir dq_data_domain_populate.sql

-- =============================================================================
-- Dimensions (Origin, Nature, Treatment)
-- =============================================================================

\echo '--- Dimensions ---'
\ir dq_origin_dimension_populate.sql
\ir dq_nature_dimension_populate.sql
\ir dq_treatment_dimension_populate.sql

-- =============================================================================
-- Subject Areas
-- =============================================================================

\echo '--- Subject Areas ---'
\ir dq_subject_area_populate.sql

-- =============================================================================
-- Coding Scheme Authority Types and Coding Schemes
-- =============================================================================

\echo '--- Coding Schemes ---'
\ir dq_coding_scheme_authority_type_populate.sql
\ir dq_coding_scheme_populate.sql

-- =============================================================================
-- General Methodologies
-- =============================================================================

\echo '--- General Methodologies ---'
\ir dq_methodology_populate.sql
