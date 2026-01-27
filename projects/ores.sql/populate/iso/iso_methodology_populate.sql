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
 * ISO Standards Methodology Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * This script is idempotent.
 */

set schema 'metadata';

-- =============================================================================
-- ISO Standards Methodologies
-- =============================================================================

\echo '--- ISO Standards Methodologies ---'

select metadata.upsert_dq_methodologies(
    'Wikipedia ISO 3166 Extraction',
    'Data extracted from Wikipedia page listing ISO 3166 country codes',
    'https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes',
    'Data sourced from https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes

See methodology documentation for detailed steps.'
);

select metadata.upsert_dq_methodologies(
    'Wikipedia ISO 4217 Extraction',
    'Data extracted from Wikipedia page listing ISO 4217 currency codes',
    'https://en.wikipedia.org/wiki/ISO_4217',
    'Data sourced from https://en.wikipedia.org/wiki/ISO_4217

See methodology documentation for detailed steps.'
);

