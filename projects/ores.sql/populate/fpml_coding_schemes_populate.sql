/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Coding Schemes Population Script
 *
 * Auto-generated from FPML Genericode XML files.
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- FPML Coding Schemes
-- =============================================================================

\echo '--- FPML Coding Schemes ---'

select ores.upsert_dq_coding_schemes(
    'FPML_NON_ISO_CURRENCY',
    'nonIsoCurrencyScheme',
    'industry',
    'Currencies',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/non-iso-currency',
    'Includes the currency codes to expand the ISO 4217 currency list, including the offshore and historical currencies.'
);
