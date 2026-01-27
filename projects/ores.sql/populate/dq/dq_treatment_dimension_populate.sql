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
 * Data Quality Treatment Dimension Population Script
 *
 * Seeds the database with treatment dimension values for data quality classification.
 * This script is idempotent.
 *
 * Treatments:
 * - Raw: Untouched, identifiable data
 * - Masked: PII has been redacted or obfuscated (replaces "Obfuscated")
 * - Anonymized: Irreversibly altered to prevent re-identification
 */

set schema 'metadata';

-- =============================================================================
-- Data Quality Treatment Dimensions
-- =============================================================================

\echo '--- Data Quality Treatment Dimensions ---'

select public.upsert_dq_treatment_dimensions(
    'Raw',
    'Raw Data',
    'Untouched, identifiable data.'
);

select public.upsert_dq_treatment_dimensions(
    'Masked',
    'Masked Data',
    'PII has been redacted or obfuscated (replaces "Obfuscated").'
);

select public.upsert_dq_treatment_dimensions(
    'Anonymized',
    'Anonymized Data',
    'Irreversibly altered to prevent re-identification.'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Treatment Dimensions' as entity, count(*) as count
from metadata.dq_treatment_dimensions_tbl where valid_to = public.utility_infinity_timestamp_fn()
order by entity;