/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * Coding Scheme Authority Type Population Script
 *
 * Seeds the database with authority type values for classifying coding schemes.
 * This script is idempotent.
 *
 * Authority Types:
 * - official: Standards body (ISO, IEEE, GLEIF)
 * - industry: De facto market standards (SWIFT, DTCC, FINRA)
 * - internal: Proprietary/organization-specific
 */

set schema 'metadata';

-- =============================================================================
-- Coding Scheme Authority Types
-- =============================================================================

\echo '--- Coding Scheme Authority Types ---'

select metadata.dq_coding_scheme_authority_types_upsert_fn(
    'official',
    'Official Standard',
    'Formal standard published by a recognized standards body such as ISO, IEEE, or a regulatory authority. These schemes have official governance, versioning, and maintenance processes.'
);

select metadata.dq_coding_scheme_authority_types_upsert_fn(
    'industry',
    'Industry Standard',
    'De facto standard widely adopted in the financial industry but not published by a formal standards body. Typically maintained by industry consortiums, trade associations, or market infrastructure providers such as SWIFT, DTCC, or FINRA.'
);

select metadata.dq_coding_scheme_authority_types_upsert_fn(
    'internal',
    'Internal/Proprietary',
    'Proprietary or organization-specific identifier scheme. Used for internal systems, client identifiers, or custom classifications that are not standardized outside the organization.'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Coding Scheme Authority Types' as entity, count(*) as count
from metadata.dq_coding_scheme_authority_types_tbl where valid_to = public.utility_infinity_timestamp_fn()
order by entity;
