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
    'FPML_PARTY_ROLE',
    'partyRoleScheme',
    'industry',
    'Reference Data',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/party-role',
    'Contains a code representing a related party role. This can be extended to provide custom roles.'
);
