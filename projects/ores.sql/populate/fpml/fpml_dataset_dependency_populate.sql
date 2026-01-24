/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Dataset Dependencies Population Script
 *
 * Auto-generated from external/fpml/manifest.json
 * Must be run after fpml dataset populate scripts.
 */

set schema 'ores';

-- =============================================================================
-- FPML Dataset Dependencies
-- =============================================================================

\echo '--- FPML Dataset Dependencies ---'

select ores.upsert_dq_dataset_dependency(
    'fpml.non_iso_currency',
    'assets.country_flags',
    'visual_assets'
);

select ores.upsert_dq_dataset_dependency(
    'fpml.business_center',
    'assets.country_flags',
    'visual_assets'
);
