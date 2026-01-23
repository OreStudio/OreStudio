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

set schema 'ores';

-- =============================================================================
-- Seed Data
-- =============================================================================

select ores.upsert_dq_methodologies(
    'Wikipedia ISO 3166 Extraction',
    'Data extracted from Wikipedia page listing ISO 3166 country codes',
    'https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes',
    'Data Sourcing and Generation Steps:

1. SOURCE DATA
   URL: https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
   Method: Manual extraction of ISO 3166-1 alpha-2, alpha-3, and numeric codes

2. POPULATE SCRIPT
   File: projects/ores.sql/populate/dq_countries_artefact_populate.sql
   Content: Manually curated SQL VALUES with country data
   Format: (alpha2_code, alpha3_code, numeric_code, name, flag_key)

3. COMMIT CHANGES
   git add projects/ores.sql/populate/dq_countries_artefact_populate.sql
   git commit -m "[sql] Update countries artefact populate script"

Countries are linked to flag images via the flag_key field, which matches
the key in the dq_images_artefact_tbl (e.g., ''gb'' -> gb.svg flag).'
);

select ores.upsert_dq_methodologies(
    'GitHub Flag Icons Download',
    'SVG images downloaded from lipis/flag-icons GitHub repository',
    'https://github.com/lipis/flag-icons',
    'Data Sourcing and Generation Steps:

1. SOURCE DATA DOWNLOAD
   Repository: https://github.com/lipis/flag-icons
   Download: Clone or download the repository
   Files: flags/4x3/*.svg (country flags in 4:3 aspect ratio)

2. SAVE TO REPOSITORY
   Target directory: projects/ores.sql/populate/data/flags/
   Copy all SVG files from flags/4x3/ to the target directory
   Commit: git add projects/ores.sql/populate/data/flags/
           git commit -m "[data] Add flag icons from lipis/flag-icons"

3. GENERATE SQL POPULATE SCRIPT
   Script: projects/ores.sql/populate/generate_dq_images_sql.py
   Command: python3 projects/ores.sql/populate/generate_dq_images_sql.py --config flags
   Output: projects/ores.sql/populate/dq_flags_images_artefact_populate.sql

4. COMMIT GENERATED SQL
   git add projects/ores.sql/populate/dq_flags_images_artefact_populate.sql
   git commit -m "[sql] Regenerate flag images populate script"'
);

select ores.upsert_dq_methodologies(
    'GitHub Cryptocurrency Icons Download',
    'SVG images downloaded from spothq/cryptocurrency-icons GitHub repository',
    'https://github.com/spothq/cryptocurrency-icons',
    'Data Sourcing and Generation Steps:

1. SOURCE DATA DOWNLOAD
   Repository: https://github.com/spothq/cryptocurrency-icons
   Commit: 1a63530be6e374711a8554f31b17e4cb92c25fa5
   Download: Clone the repository at the specified commit
   Files: svg/color/*.svg (colored cryptocurrency icons)

2. SAVE TO REPOSITORY
   Target directory: projects/ores.sql/populate/data/cryptocurrency-icons/
   Copy all SVG files from svg/color/ to the target directory
   Commit: git add projects/ores.sql/populate/data/cryptocurrency-icons/
           git commit -m "[data] Add cryptocurrency icons from spothq/cryptocurrency-icons"

3. GENERATE SQL POPULATE SCRIPT
   Script: projects/ores.sql/populate/generate_dq_images_sql.py
   Command: python3 projects/ores.sql/populate/generate_dq_images_sql.py --config crypto
   Output: projects/ores.sql/populate/dq_crypto_images_artefact_populate.sql

4. COMMIT GENERATED SQL
   git add projects/ores.sql/populate/dq_crypto_images_artefact_populate.sql
   git commit -m "[sql] Regenerate cryptocurrency icons populate script"'
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
   File: projects/ores.sql/populate/dq_currencies_artefact_populate.sql
   Content: Manually curated SQL VALUES with currency data
   Format: (iso_code, name, numeric_code, symbol, fraction_symbol, fractions_per_unit,
            rounding_type, rounding_precision, format, currency_type, flag_key)

3. COMMIT CHANGES
   git add projects/ores.sql/populate/dq_currencies_artefact_populate.sql
   git commit -m "[sql] Update currencies artefact populate script"

Fiat currencies are classified as ''fiat.major'' (EUR, USD, GBP, JPY, AUD, CAD, CHF,
DKK, NOK, NZD, SEK) or ''fiat.emerging'' (all others). Commodity currencies (XAU, XAG, etc.)
use type ''commodity''. SDR uses type ''supranational''.'
);

select ores.upsert_dq_methodologies(
    'GitHub Cryptocurrencies JSON Download',
    'Cryptocurrency symbol-to-name mappings from crypti/cryptocurrencies GitHub repository',
    'https://github.com/crypti/cryptocurrencies',
    'Data Sourcing and Generation Steps:

1. SOURCE DATA DOWNLOAD
   Repository: https://github.com/crypti/cryptocurrencies
   File: cryptocurrencies.json
   Download: curl -o cryptocurrencies.json https://raw.githubusercontent.com/crypti/cryptocurrencies/master/cryptocurrencies.json

2. SAVE TO REPOSITORY
   Target directory: projects/ores.sql/populate/data/cryptocurrencies/
   Target file: cryptocurrencies.json
   Commit: git add projects/ores.sql/populate/data/cryptocurrencies/cryptocurrencies.json
           git commit -m "[data] Add cryptocurrencies JSON from crypti/cryptocurrencies"

3. GENERATE SQL POPULATE SCRIPT
   Script: projects/ores.sql/populate/generate_dq_cryptocurrencies_sql.py
   Command: python3 projects/ores.sql/populate/generate_dq_cryptocurrencies_sql.py
   Optional arguments:
     --source-file PATH    Path to cryptocurrencies.json
     --output-file PATH    Output SQL file path
   Output: projects/ores.sql/populate/dq_cryptocurrencies_artefact_populate.sql

4. COMMIT GENERATED SQL
   git add projects/ores.sql/populate/dq_cryptocurrencies_artefact_populate.sql
   git commit -m "[sql] Regenerate cryptocurrencies populate script"

The script classifies cryptocurrencies as ''crypto.major'' (top 20 by market cap)
or ''crypto.minor'' (all others). Only crypto.major currencies are populated to production.'
);

select ores.upsert_dq_methodologies(
    'FpML Genericode Download',
    'Data downloaded from FpML coding scheme repository in Genericode XML format',
    'http://www.fpml.org/coding-scheme/',
    'Data Sourcing and Generation Steps:

1. SOURCE DATA DOWNLOAD
   Repository: http://www.fpml.org/coding-scheme/
   Format: Genericode XML (OASIS standard for code lists)
   Download: curl -o <scheme-name>.xml http://www.fpml.org/coding-scheme/<scheme-name>.xml
   Example: curl -o non-iso-currency-1-1.xml http://www.fpml.org/coding-scheme/non-iso-currency-1-1.xml

2. SAVE TO REPOSITORY
   Target directory: external/fpml/codelist/
   Example: external/fpml/codelist/non-iso-currency-1-1.xml
   Commit: git add external/fpml/codelist/<scheme-name>.xml
           git commit -m "[data] Add FpML <scheme-name> genericode file"

3. GENERATE SQL FILES
   Script: projects/ores.codegen/generate_fpml_refdata.sh
   Command: ./generate_fpml_refdata.sh --entities "<entity-type>"
   Output: Schema and populate SQL files in projects/ores.sql/

4. COMMIT GENERATED SQL
   git add projects/ores.sql/schema/refdata_<entity>*.sql
   git add projects/ores.sql/populate/dq_<entity>*.sql fpml_<entity>*.sql
   git commit -m "[sql] Regenerate <entity> files from FpML codegen"

FpML Genericode files follow the OASIS CodeList standard. Each file contains
Code, Source, and Description columns. The CanonicalVersionUri identifies the specific
version of the coding scheme.'
);

select ores.upsert_dq_methodologies(
    'iptoasn.com IP to Country Download',
    'IPv4 to country mapping data downloaded from iptoasn.com, a community-maintained database by Frank Denis',
    'https://iptoasn.com/',
    'Data Sourcing and Import Steps:

1. SOURCE DATA DOWNLOAD
   Website: https://iptoasn.com/
   Creator: Frank Denis
   License: PDDL v1.0 (Public Domain)
   Update frequency: Hourly
   File: ip2country-v4-u32.tsv.gz (IPv4 in 32-bit unsigned integer format)
   Download: curl -o ip2country-v4-u32.tsv.gz https://iptoasn.com/data/ip2country-v4-u32.tsv.gz
             gunzip ip2country-v4-u32.tsv.gz

2. SAVE TO REPOSITORY
   Target directory: projects/ores.sql/populate/data/ip2country/
   Target file: ip2country-v4-u32.tsv
   Format: TSV with columns (range_start, range_end, country_code)
   Commit: git add projects/ores.sql/populate/data/ip2country/ip2country-v4-u32.tsv
           git commit -m "[data] Update IP to country mapping from iptoasn.com"

3. LOAD TO DQ STAGING TABLE
   Script: projects/ores.sql/populate/dq_ip2country_artefact_populate.sql
   Target table: ores.dq_ip2country_artefact_tbl
   Process: Uses psql \copy to import TSV directly into staging table
   Columns: (dataset_id, range_start, range_end, country_code)

4. POPULATE PRODUCTION TABLE
   Function: ores.dq_populate_ip2country(p_dataset_id, ''replace_all'')
   Source: dq_ip2country_artefact_tbl (staging)
   Target: geo_ip2country_tbl (production)
   Process: Truncate production, insert from staging, convert to int8range, analyze

5. VERIFY IMPORT
   Test lookups for known IPs (8.8.8.8 -> US, 1.1.1.1 -> US)
   Check statistics: total ranges, unique countries, unrouted ranges'
);

select ores.upsert_dq_methodologies(
    'Synthetic Data Generation',
    'Test data generated programmatically using the ores.synthetic library with seeded random generation',
    'https://github.com/cieslarmichal/faker-cxx',
    'Data Generation Approach:

1. LIBRARY
   Component: ores.synthetic
   Dependencies: faker-cxx library for realistic random data generation

2. GENERATION PROCESS
   - Uses seeded random number generator for reproducibility
   - Generates coherent datasets with proper entity relationships
   - All foreign key references point to valid entities
   - Timestamps and audit fields populated consistently

3. REPRODUCIBILITY
   - Same seed produces identical output
   - Seed value should be tracked by caller for reproducibility
   - Default uses random seed if not specified

4. USAGE
   Code: ores::synthetic::service::catalog_generator_service
   Options: ores::synthetic::domain::generation_options

This methodology is used for all programmatically generated test data.
The specific seed and generation parameters are tracked separately from
the methodology itself.'
);

-- =============================================================================
-- Summary
-- =============================================================================

select 'dq_methodologies' as entity, count(*) as count
from ores.dq_methodologies_tbl
where valid_to = ores.utility_infinity_timestamp_fn();
