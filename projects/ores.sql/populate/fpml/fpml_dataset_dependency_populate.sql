/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Dataset Dependencies Population Script
 *
 * Auto-generated from external/fpml/manifest.json
 * Must be run after fpml dataset populate scripts.
 */

DO $$
BEGIN
    -- =============================================================================
    -- FPML Dataset Dependencies
    -- =============================================================================

    -- --- FPML Dataset Dependencies ---

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'fpml.non_iso_currency',
        'assets.country_flags',
        'visual_assets'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'fpml.business_center',
        'assets.country_flags',
        'visual_assets'
    );
END $$;

