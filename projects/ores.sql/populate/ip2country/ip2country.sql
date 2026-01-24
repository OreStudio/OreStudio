/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * IP to Country Reference Data Master Include File
 *
 * Includes all IP to country SQL files in the correct dependency order.
 * Data sourced from iptoasn.com (PDDL v1.0 license).
 */

-- =============================================================================
-- IP Geolocation Catalog (must come first)
-- =============================================================================

\echo '--- IP Geolocation Catalog ---'
\ir ip2country_catalog_populate.sql

-- =============================================================================
-- IP to Country Artefacts
-- =============================================================================

\echo '--- IP to Country Artefacts ---'
\ir dq_ip2country_artefact_populate.sql
