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
 * ISO Standards Coding Schemes Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- ISO Standards Coding Schemes
-- =============================================================================

\echo '--- ISO Standards Coding Schemes ---'

select ores.upsert_dq_coding_schemes(
    'ISO_3166_1_ALPHA_2',
    'ISO 3166-1 Alpha-2 Country Code',
    'official',
    'Countries',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso3166-1-alpha-2',
    'ISO 3166-1 alpha-2 country codes. Two-letter codes (e.g., US, GB, DE) for countries and dependent territories. The most commonly used country code format in financial messaging.'
);

select ores.upsert_dq_coding_schemes(
    'ISO_3166_1_ALPHA_3',
    'ISO 3166-1 Alpha-3 Country Code',
    'official',
    'Countries',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso3166-1-alpha-3',
    'ISO 3166-1 alpha-3 country codes. Three-letter codes (e.g., USA, GBR, DEU) for countries and dependent territories. More descriptive than alpha-2 codes.'
);

select ores.upsert_dq_coding_schemes(
    'ISO_4217',
    'ISO 4217 Currency Code',
    'official',
    'Currencies',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso4217',
    'ISO 4217 currency codes. Three-letter alphabetic codes (e.g., USD, EUR, GBP) and three-digit numeric codes for currencies. The universal standard for currency identification in financial transactions.'
);

