/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Variability (Feature Flags) Master Include File
 *
 * Includes all variability SQL files.
 */

-- =============================================================================
-- System Flags
-- =============================================================================

\echo '--- System Flags ---'
\ir variability_system_flags_populate.sql
