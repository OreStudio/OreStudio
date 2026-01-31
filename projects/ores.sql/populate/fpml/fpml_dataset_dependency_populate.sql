/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Dataset Dependencies Population Script
 *
 * Auto-generated from external/fpml/manifest.json
 * Must be run after fpml dataset populate scripts.
 */

-- =============================================================================
-- FPML Dataset Dependencies
-- =============================================================================

\echo '--- FPML Dataset Dependencies ---'

select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'fpml.non_iso_currency',
    'assets.country_flags',
    'visual_assets'
);

select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'fpml.business_center',
    'assets.country_flags',
    'visual_assets'
);
