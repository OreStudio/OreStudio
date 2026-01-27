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

set schema 'metadata';

-- =============================================================================
-- Data Quality Slovaris Methodologies
-- =============================================================================

\echo '--- Data Quality Slovaris Methodologies ---'

select public.upsert_dq_methodologies(
    'OreStudio Code Generation Methodology',
    'Methodology for generating artefact files using the OreStudio code generator',
    'https://github.com/OreStudio/OreStudio/tree/main/projects/ores.codegen',
    'Code Generation Methodology:

This document describes the methodology for generating multiple artefact files using the OreStudio code generator. Each section corresponds to a different generated artefact type.

CATALOGS ARTEFACT GENERATION:

1. SOURCE MODEL DATA
   File: projects/ores.codegen/models/slovaris/catalogs.json
   Method: JSON structured data containing catalog information
   Format: { "name": "Slovaris", "description": "Imaginary world...", "owner": "Testing Team" }

2. DEFINE TEMPLATE
   File: projects/ores.codegen/library/templates/sql_catalog_populate.mustache
   Content: Mustache template for generating SQL UPSERT statements for catalogs
   Format: Uses {{#catalogs}}...{{/catalogs}} loop structure

3. EXECUTE GENERATION
   Command: ./run_generator.sh models/slovaris/catalogs.json
   Output: Generated SQL file with proper license headers and modelines
   Location: projects/ores.codegen/output/

4. INTEGRATE WITH SYSTEM
   Target: projects/ores.sql/populate/dq_catalogs_artefact_populate.sql
   Method: Copy generated content to appropriate system location
   Format: Follows system conventions for data quality artefacts

5. COMMIT CHANGES
   git add projects/ores.sql/populate/dq_catalogs_artefact_populate.sql
   git commit -m "[codegen] Add catalogs artefact populate script from generated code"

Country currencies are linked to countries via the country_code field, which matches
the alpha2_code in the dq_countries_artefact_tbl (e.g., ''US'' -> US Dollar).
The generator automatically includes proper licensing headers and editor modelines
based on the template and data configurations in the codegen project.

COUNTRY CURRENCY ARTEFACT GENERATION:

1. SOURCE MODEL DATA
   File: projects/ores.codegen/models/slovaris/country_currency.json
   Method: JSON structured data containing currency information for countries
   Format: { "country_code": "US", "currency_code": "USD", "currency_name": "US Dollar", "symbol": "$" }

2. DEFINE TEMPLATE
   File: projects/ores.codegen/library/templates/sql_country_currency_populate.mustache
   Content: Mustache template for generating SQL INSERT/UPSERT statements
   Format: Uses {{#country_currencies}}...{{/country_currencies}} loop structure

3. EXECUTE GENERATION
   Command: ./run_generator.sh models/slovaris/country_currency.json
   Output: Generated SQL file with proper license headers and modelines
   Location: projects/ores.codegen/output/

4. INTEGRATE WITH SYSTEM
   Target: projects/ores.sql/populate/dq_country_currency_artefact_populate.sql
   Method: Copy generated content to appropriate system location
   Format: Follows system conventions for data quality artefacts

5. COMMIT CHANGES
   git add projects/ores.sql/populate/dq_country_currency_artefact_populate.sql
   git commit -m "[codegen] Add country currency artefact populate script from generated code"

Country currencies are linked to countries via the country_code field, which matches
the alpha2_code in the dq_countries_artefact_tbl (e.g., ''US'' -> US Dollar).
The generator automatically includes proper licensing headers and editor modelines
based on the template and data configurations in the codegen project.

DATASETS ARTEFACT GENERATION:

1. SOURCE MODEL DATA
   File: projects/ores.codegen/models/slovaris/datasets.json
   Method: JSON structured data containing dataset information
   Format: { "name": "Dataset Name", "description": "Dataset Description", "catalog_id": "Catalog ID" }

2. DEFINE TEMPLATE
   File: projects/ores.codegen/library/templates/sql_dataset_populate.mustache
   Content: Mustache template for generating SQL UPSERT statements for datasets
   Format: Uses {{#datasets}}...{{/datasets}} loop structure

3. EXECUTE GENERATION
   Command: ./run_generator.sh models/slovaris/datasets.json
   Output: Generated SQL file with proper license headers and modelines
   Location: projects/ores.codegen/output/

4. INTEGRATE WITH SYSTEM
   Target: projects/ores.sql/populate/dq_datasets_artefact_populate.sql
   Method: Copy generated content to appropriate system location
   Format: Follows system conventions for data quality artefacts

5. COMMIT CHANGES
   git add projects/ores.sql/populate/dq_datasets_artefact_populate.sql
   git commit -m "[codegen] Add datasets artefact populate script from generated code"

Datasets are linked to catalogs via the catalog_id field, which matches
the name in the dq_catalogs_tbl (e.g., ''Slovaris'' -> related datasets).
The generator automatically includes proper licensing headers and editor modelines
based on the template and data configurations in the codegen project.

EXTENDING THE METHODOLOGY:

As new artefact types are added to the code generator:
1. Add a new section following the same format
2. Include the appropriate model file and template
3. Document the integration target and commit message convention
4. Describe any relationships to other artefacts'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Total Methodologies' as entity, count(*) as count
from metadata.dq_methodologies_tbl where valid_to = public.utility_infinity_timestamp_fn()
order by entity;
