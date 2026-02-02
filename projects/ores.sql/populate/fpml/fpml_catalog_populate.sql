/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Catalog Population Script
 *
 * Auto-generated from external/fpml/manifest.json
 * This script is idempotent.
 */



-- =============================================================================
-- FpML Standards Catalog
-- =============================================================================

\echo '--- FpML Standards Catalog ---'

select ores_dq_catalogs_upsert_fn(ores_iam_system_tenant_id_fn(),
    'FpML Standards',
    'Financial products Markup Language (FpML) coding schemes and reference data for OTC derivatives trading. Includes non-ISO currencies, business centers, and other FpML-defined code lists.',
    'Reference Data Team'
);
