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
 * Flag Icons Methodology Population Script
 *
 * Auto-generated from external/flags/manifest.json
 * This script is idempotent.
 */

set schema 'metadata';

-- =============================================================================
-- Flag Icons Data Sourcing Methodologies
-- =============================================================================

\echo '--- Flag Icons Methodologies ---'

select metadata.upsert_dq_methodologies(
    'GitHub Flag Icons Download',
    'SVG images downloaded from lipis/flag-icons GitHub repository',
    'https://github.com/lipis/flag-icons',
    'Last Download: 2024-12-29

Data Sourcing and Generation Steps:

1. SOURCE DATA DOWNLOAD
   Repository: https://github.com/lipis/flag-icons
   Download: Clone or download the repository
   Files: flags/4x3/*.svg (country flags in 4:3 aspect ratio)

2. SAVE TO REPOSITORY
   Target directory: external/flags/flag-icons/
   Copy all SVG files from flags/4x3/ to the target directory
   Commit: git add external/flags/flag-icons/
           git commit -m "[data] Add flag icons from lipis/flag-icons"

3. GENERATE SQL POPULATE SCRIPT
   Script: projects/ores.codegen/src/images_generate_sql.py
   Command: python3 images_generate_sql.py --config flags
   Output: projects/ores.sql/populate/flags/flags_images_artefact_populate.sql

4. COMMIT GENERATED SQL
   git add projects/ores.sql/populate/flags/
   git commit -m "[sql] Regenerate flag images populate script"

FLAGS STRUCTURE
---------------
Output files (in projects/ores.sql/populate/flags/):
  - flags.sql (master include)
  - flags_methodology_populate.sql (methodology definitions)
  - flags_dataset_populate.sql (dataset definitions)
  - flags_dataset_tag_populate.sql (dataset tags)
  - flags_images_artefact_populate.sql (SVG image data)

FLAG KEYING
-----------
Flags are keyed by lowercase ISO 3166-1 alpha-2 code (e.g., ''gb'', ''us'', ''fr'').
When populating countries, the script joins to the images dataset
to link each country to its flag. The key matches the SVG filename
without extension (e.g., ''gb.svg'' -> key ''gb'').

SPECIAL FLAGS
-------------
Some flags are for non-country entities:
  - eu.svg: European Union
  - un.svg: United Nations
  - arab.svg: Arab League
  - asean.svg: ASEAN
  - cefta.svg: CEFTA
  - eac.svg: East African Community
  - pc.svg: Pacific Community
  - xx.svg: Unknown/placeholder'
);

