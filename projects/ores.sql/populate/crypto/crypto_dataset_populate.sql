/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
 * Cryptocurrency Dataset Population Script
 *
 * Creates the dataset entries for cryptocurrency reference data.
 * Auto-generated from external/crypto/manifest.json
 * This must be run before populating the artefact tables.
 */

set schema 'metadata';

-- =============================================================================
-- Cryptocurrency Datasets
-- =============================================================================

\echo '--- Cryptocurrency Datasets ---'

-- Cryptocurrency Icon Images
select metadata.upsert_dq_datasets(
    'assets.crypto_icons',
    'Cryptocurrency',
    'Cryptocurrencies',
    'Reference Data',
    'NONE',
    'Primary',
    'Actual',
    'Raw',
    'GitHub Cryptocurrency Icons Download',
    'Cryptocurrency Icon Images',
    'SVG icon images for cryptocurrency symbols from the cryptocurrency-icons project.',
    'spothq/cryptocurrency-icons',
    'Visual assets for cryptocurrency displays',
    '2024-01-15'::date,
    'MIT License',
    'images'
);

-- Cryptocurrencies Large
select metadata.upsert_dq_datasets(
    'crypto.large',
    'Cryptocurrency',
    'Cryptocurrencies',
    'Reference Data',
    'NONE',
    'Primary',
    'Actual',
    'Raw',
    'GitHub Cryptocurrencies JSON Download',
    'Cryptocurrencies Large',
    'Complete list of ~12,243 cryptocurrencies with symbol and name.',
    'crypti/cryptocurrencies',
    'Full cryptocurrency reference data (~12K coins)',
    '2026-01-20'::date,
    'Public Domain',
    'currencies'
);

-- Cryptocurrencies Small
select metadata.upsert_dq_datasets(
    'crypto.small',
    'Cryptocurrency',
    'Cryptocurrencies',
    'Reference Data',
    'NONE',
    'Primary',
    'Actual',
    'Raw',
    'GitHub Cryptocurrencies JSON Download',
    'Cryptocurrencies Small',
    'Top 100 cryptocurrencies by market capitalization.',
    'crypti/cryptocurrencies',
    'Top 100 cryptocurrency reference data by market cap',
    '2026-01-20'::date,
    'Public Domain',
    'currencies'
);

