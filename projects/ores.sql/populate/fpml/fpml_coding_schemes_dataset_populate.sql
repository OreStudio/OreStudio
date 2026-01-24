/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Coding Schemes Dataset Population Script
 *
 * Auto-generated from external/fpml/manifest.json
 * This must be run before other datasets that reference these coding schemes.
 */

set schema 'ores';

-- =============================================================================
-- FPML Coding Schemes Dataset
-- =============================================================================

\echo '--- FPML Coding Schemes Dataset ---'

-- FpML Coding Schemes
select ores.upsert_dq_datasets(
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
    'FpML License',
    'coding_schemes',
    'dq_coding_schemes_tbl',
    'dq_populate_coding_schemes'
);
