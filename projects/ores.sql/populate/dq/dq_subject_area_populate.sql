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
 * Data Quality Subject Area Population Script
 *
 * Seeds the database with subject area values for data quality classification.
 * This script is idempotent.
 *
 * Subject Areas:
 * - Currencies: Currency reference data
 * - Countries: Country reference data
 * - Country Flags: Flag image data associated with countries
 * - IP Address to Country maps: IP geolocation mapping data
 * - Cryptocurrencies: Cryptocurrency reference data including icons
 * - Parties: Party identification schemes and entity classifications
 * - Trading: Trading infrastructure (accounts, business centres, cashflows)
 * - Market Data: Asset classes, measures, and benchmark rates
 * - Regulatory: Regulatory bodies, regimes, and jurisdictions
 * - General: Cross-cutting reference data
 */

set schema 'ores';

-- =============================================================================
-- Data Quality Subject Areas
-- =============================================================================

\echo '--- Data Quality Subject Areas ---'

select ores.upsert_dq_subject_areas(
    'Reference Data',
    'Currencies',
    'Currency reference data.'
);

select ores.upsert_dq_subject_areas(
    'Reference Data',
    'Countries',
    'Country reference data.'
);

select ores.upsert_dq_subject_areas(
    'Reference Data',
    'Country Flags',
    'Flag image data associated with countries.'
);

select ores.upsert_dq_subject_areas(
    'Reference Data',
    'IP Address to Country maps',
    'IP geolocation mapping data.'
);

select ores.upsert_dq_subject_areas(
    'Reference Data',
    'Cryptocurrencies',
    'Cryptocurrency reference data including icons and metadata.'
);

select ores.upsert_dq_subject_areas(
    'Reference Data',
    'Parties',
    'Party identification schemes and reference data for legal entities and financial institutions.'
);

select ores.upsert_dq_subject_areas(
    'Reference Data',
    'General',
    'Cross-cutting reference data not specific to a particular domain.'
);

select ores.upsert_dq_subject_areas(
    'Reference Data',
    'Trading',
    'Trading infrastructure reference data including account types, business centres, and cashflow types.'
);

select ores.upsert_dq_subject_areas(
    'Reference Data',
    'Market Data',
    'Market data reference including asset classes, asset measures, and benchmark rates.'
);

select ores.upsert_dq_subject_areas(
    'Reference Data',
    'Regulatory',
    'Regulatory reference data including supervisory bodies, reporting regimes, and jurisdictions.'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Subject Areas' as entity, count(*) as count
from ores.dq_subject_areas_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;