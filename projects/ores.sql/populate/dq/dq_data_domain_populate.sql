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
 * Data Quality Data Domain Population Script
 *
 * Seeds the database with data domain values for data quality classification.
 * This script is idempotent.
 *
 * Domains:
 * - Reference Data: Standardized data used across the system
 * - Trade Data: Transaction and position data
 * - Market Data: Pricing and market information
 */

set schema 'metadata';

-- =============================================================================
-- Data Quality Data Domains
-- =============================================================================

\echo '--- Data Quality Data Domains ---'

select metadata.upsert_dq_data_domains(
    'Reference Data',
    'Standardized data used across the system.'
);

select metadata.upsert_dq_data_domains(
    'Trade Data',
    'Transaction and position data.'
);

select metadata.upsert_dq_data_domains(
    'Market Data',
    'Pricing and market information.'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Data Domains' as entity, count(*) as count
from metadata.dq_data_domains_tbl where valid_to = public.utility_infinity_timestamp_fn()
order by entity;