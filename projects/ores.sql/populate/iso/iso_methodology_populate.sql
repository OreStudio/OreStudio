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
 * ISO Standards Methodology Population Script
 *
 * Data sourcing methodologies for ISO reference data.
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- ISO Standards Methodologies
-- =============================================================================

\echo '--- ISO Standards Methodologies ---'

select ores.upsert_dq_methodologies(
    'Wikipedia ISO 3166 Extraction',
    'Data extracted from Wikipedia page listing ISO 3166 country codes',
    'https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes',
    'Data Sourcing and Generation Steps:

1. SOURCE DATA
   URL: https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
   Method: Manual extraction of ISO 3166-1 alpha-2, alpha-3, and numeric codes

2. POPULATE SCRIPT
   File: projects/ores.sql/populate/iso/iso_countries_artefact_populate.sql
   Content: Manually curated SQL VALUES with country data
   Format: (alpha2_code, alpha3_code, numeric_code, name, flag_key)

3. COMMIT CHANGES
   git add projects/ores.sql/populate/iso/iso_countries_artefact_populate.sql
   git commit -m "[sql] Update countries artefact populate script"

Countries are linked to flag images via the flag_key field, which matches
the key in the dq_images_artefact_tbl (e.g., ''gb'' -> gb.svg flag).'
);

select ores.upsert_dq_methodologies(
    'Wikipedia ISO 4217 Extraction',
    'Data extracted from Wikipedia page listing ISO 4217 currency codes',
    'https://en.wikipedia.org/wiki/ISO_4217',
    'Data Sourcing and Generation Steps:

1. SOURCE DATA
   URL: https://en.wikipedia.org/wiki/ISO_4217
   Method: Manual extraction of ISO 4217 currency codes with names, symbols, and formatting

2. POPULATE SCRIPT
   File: projects/ores.sql/populate/iso/iso_currencies_artefact_populate.sql
   Content: Manually curated SQL VALUES with currency data
   Format: (iso_code, name, numeric_code, symbol, fraction_symbol, fractions_per_unit,
            rounding_type, rounding_precision, format, currency_type, flag_key)

3. COMMIT CHANGES
   git add projects/ores.sql/populate/iso/iso_currencies_artefact_populate.sql
   git commit -m "[sql] Update currencies artefact populate script"

Fiat currencies are classified as ''fiat.major'' (EUR, USD, GBP, JPY, AUD, CAD, CHF,
DKK, NOK, NZD, SEK) or ''fiat.emerging'' (all others). Commodity currencies (XAU, XAG, etc.)
use type ''commodity''. SDR uses type ''supranational''.'
);
