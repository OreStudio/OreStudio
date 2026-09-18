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
 * Auto-generated from external/iso/manifest.json and methodology.txt
 * This script is idempotent.
 */

DO $$
BEGIN
    -- =============================================================================
    -- ISO Standards Methodologies
    -- =============================================================================

    -- --- ISO Standards Methodologies ---

    PERFORM ores_dq_methodologies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'Wikipedia ISO 3166 Extraction',
        'Data extracted from Wikipedia page listing ISO 3166 country codes',
        'https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes',
        'Data Sourcing and Generation Steps:

1. NO SOURCE DATA DOWNLOAD
   This dataset has no source data file. Nothing is downloaded, because
   the ISO standards are published as documents and not as a machine
   readable list that this repository may redistribute.

2. DATA ENTRY
   The country and currency rows were entered by hand from the standards:
     ISO 3166-1  country codes
     ISO 4217   currency codes

   No file sits between the standards and the SQL. The other datasets
   download a file and derive their rows from it. ISO cannot work that
   way, which is why the two artefact files are maintained by hand.

3. GENERATE THE METADATA SQL
   Script: external/iso/iso_generate_metadata_sql.py
   Command: python3 external/iso/iso_generate_metadata_sql.py
   Input: external/iso/manifest.json and this file
   Output: projects/ores.sql/populate/iso/

   The command needs no arguments. It reads the manifest and this file
   from its own directory, and writes to the standard populate
   directory. Run it after every change to manifest.json or to this
   file: the text of this file is embedded in iso_methodology_populate.sql.

4. THE ARTEFACT SQL IS MAINTAINED BY HAND
   Two files have no generator:
     iso_countries_artefact_populate.sql
     iso_currencies_artefact_populate.sql

   Nothing derives them. A new country, or a change to a currency, is an
   edit to those files.

5. COMMIT
   git add external/iso/ projects/ores.sql/populate/iso/
   git commit -m "[sql] Update ISO populate scripts"

ISO STRUCTURE
-------------
Output files (in projects/ores.sql/populate/iso/):
  - iso_populate.sql (master include)
  - iso_catalog_populate.sql
  - iso_coding_schemes_artefact_populate.sql
  - iso_coding_schemes_dataset_populate.sql
  - iso_countries_artefact_populate.sql (hand-written)
  - iso_currencies_artefact_populate.sql (hand-written)
  - iso_dataset_dependency_populate.sql
  - iso_dataset_populate.sql
  - iso_dataset_tag_populate.sql
  - iso_methodology_populate.sql

MAINTENANCE
-----------
To add a country or a currency, edit the two hand-written files. To
change the catalogue, the coding schemes or a dataset description, edit
manifest.json and run the generator. The standards are the authority and
the SQL is the transcription of them.'
    );

    PERFORM ores_dq_methodologies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'Wikipedia ISO 4217 Extraction',
        'Data extracted from Wikipedia page listing ISO 4217 currency codes',
        'https://en.wikipedia.org/wiki/ISO_4217',
        'Data Sourcing and Generation Steps:

1. NO SOURCE DATA DOWNLOAD
   This dataset has no source data file. Nothing is downloaded, because
   the ISO standards are published as documents and not as a machine
   readable list that this repository may redistribute.

2. DATA ENTRY
   The country and currency rows were entered by hand from the standards:
     ISO 3166-1  country codes
     ISO 4217   currency codes

   No file sits between the standards and the SQL. The other datasets
   download a file and derive their rows from it. ISO cannot work that
   way, which is why the two artefact files are maintained by hand.

3. GENERATE THE METADATA SQL
   Script: external/iso/iso_generate_metadata_sql.py
   Command: python3 external/iso/iso_generate_metadata_sql.py
   Input: external/iso/manifest.json and this file
   Output: projects/ores.sql/populate/iso/

   The command needs no arguments. It reads the manifest and this file
   from its own directory, and writes to the standard populate
   directory. Run it after every change to manifest.json or to this
   file: the text of this file is embedded in iso_methodology_populate.sql.

4. THE ARTEFACT SQL IS MAINTAINED BY HAND
   Two files have no generator:
     iso_countries_artefact_populate.sql
     iso_currencies_artefact_populate.sql

   Nothing derives them. A new country, or a change to a currency, is an
   edit to those files.

5. COMMIT
   git add external/iso/ projects/ores.sql/populate/iso/
   git commit -m "[sql] Update ISO populate scripts"

ISO STRUCTURE
-------------
Output files (in projects/ores.sql/populate/iso/):
  - iso_populate.sql (master include)
  - iso_catalog_populate.sql
  - iso_coding_schemes_artefact_populate.sql
  - iso_coding_schemes_dataset_populate.sql
  - iso_countries_artefact_populate.sql (hand-written)
  - iso_currencies_artefact_populate.sql (hand-written)
  - iso_dataset_dependency_populate.sql
  - iso_dataset_populate.sql
  - iso_dataset_tag_populate.sql
  - iso_methodology_populate.sql

MAINTENANCE
-----------
To add a country or a currency, edit the two hand-written files. To
change the catalogue, the coding schemes or a dataset description, edit
manifest.json and run the generator. The standards are the authority and
the SQL is the transcription of them.'
    );
END $$;

