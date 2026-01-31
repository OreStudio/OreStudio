/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Coding Schemes Dataset Population Script
 *
 * Auto-generated from external/fpml/manifest.json
 * This must be run before other datasets that reference these coding schemes.
 */

-- =============================================================================
-- FPML Coding Schemes Dataset
-- =============================================================================

\echo '--- FPML Coding Schemes Dataset ---'

-- FpML Coding Schemes
select ores_dq_datasets_upsert_fn(
    'fpml.coding_schemes',
    'FpML Standards',
    'General',
    'Reference Data',
    'NONE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Coding Schemes',
    'FpML coding scheme definitions for account types, asset classes, business centers, and other reference data.',
    'FPML',
    'Coding scheme metadata for FpML standards',
    current_date,
    'FpML License',
    'coding_schemes'
);
