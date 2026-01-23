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
 * Cryptocurrency Dataset Tags
 *
 * Tags for cryptocurrency datasets.
 * Auto-generated from external/crypto/manifest.json
 * Must be run after crypto_dataset_populate.sql.
 */

set schema 'ores';

-- =============================================================================
-- Cryptocurrency Dataset Tags
-- =============================================================================

\echo '--- Cryptocurrency Dataset Tags ---'

select ores.upsert_dq_tag(
    'Cryptocurrency Icon Images',
    'Cryptocurrencies',
    'Reference Data',
    'cryptocurrency',
    'Cryptocurrency icon images'
);

select ores.upsert_dq_tag(
    'Cryptocurrencies Large',
    'Cryptocurrencies',
    'Reference Data',
    'cryptocurrency',
    'Cryptocurrency reference data (~12K coins)'
);

select ores.upsert_dq_tag(
    'Cryptocurrencies Small',
    'Cryptocurrencies',
    'Reference Data',
    'cryptocurrency',
    'Top 100 cryptocurrencies by market cap'
);

