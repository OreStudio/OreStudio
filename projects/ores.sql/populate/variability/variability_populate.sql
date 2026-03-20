/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Variability (Feature Flags) Master Include File
 *
 * Includes all variability SQL files.
 */

-- =============================================================================
-- System Flags (legacy - kept until flags table is dropped)
-- =============================================================================

\echo '--- System Flags ---'
\ir variability_system_flags_populate.sql

-- =============================================================================
-- System Settings (unified typed table)
-- =============================================================================

\echo '--- System Settings ---'
\ir variability_system_settings_populate.sql
