/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Catalog Population Script
 *
 * Auto-generated from external/fpml/manifest.json
 * This script is idempotent.
 */

set schema 'metadata';

-- =============================================================================
-- FpML Standards Catalog
-- =============================================================================

\echo '--- FpML Standards Catalog ---'

select metadata.upsert_dq_catalogs(
    'FpML Standards',
    'Financial products Markup Language (FpML) coding schemes and reference data for OTC derivatives trading. Includes non-ISO currencies, business centers, and other FpML-defined code lists.',
    'Reference Data Team'
);
