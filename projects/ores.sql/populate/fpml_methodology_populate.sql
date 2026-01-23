/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Methodology Population Script
 *
 * Auto-generated from external/fpml/manifest.json
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- FPML Data Sourcing Methodology
-- =============================================================================

\echo '--- FPML Methodology ---'

select ores.upsert_dq_methodologies(
    'FpML Genericode Download',
    'Data downloaded from FpML coding scheme repository in Genericode XML format',
    'https://www.fpml.org/the_standard/current/',
    'Last Download: 2026-01-23

Data Sourcing and Generation Steps:

1. SOURCE DATA DOWNLOAD
   Specification: https://www.fpml.org/the_standard/current/
   Format: Genericode XML (OASIS standard for code lists)
   Download: curl -o codelist.zip https://www.fpml.org/spec/coding-scheme/codelist.zip
   Unpack: unzip codelist.zip -d external/fpml/codelist/

2. SAVE TO REPOSITORY
   Target directory: external/fpml/codelist/
   Commit: git add external/fpml/codelist/
           git commit -m "[data] Update FpML genericode files"

3. GENERATE SQL FILES
   Script: projects/ores.codegen/generate_fpml_refdata.sh
   Command: ./generate_fpml_refdata.sh
   Output: Schema and populate SQL files in projects/ores.sql/

4. COMMIT GENERATED SQL
   git add projects/ores.sql/schema/refdata_<entity>*.sql
   git add projects/ores.sql/populate/fpml*.sql dq_<entity>*.sql
   git commit -m "[sql] Regenerate FPML reference data files"

FpML Genericode files follow the OASIS CodeList standard. Each file contains
Code, Source, and Description columns. The CanonicalVersionUri identifies the
specific version of the coding scheme.'
);
