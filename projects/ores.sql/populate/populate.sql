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
 * System Population Script
 *
 * Seeds the database with governance and catalogue data. Foundation layer
 * data (change reasons, IAM, coding schemes, etc.) is already included in
 * the database template via setup_template.sql.
 *
 * Population Layers:
 *
 * 1. Foundation Layer (already in template - not run here)
 *    - Change Control: Categories and reasons for audit trail
 *    - Reference Data Lookup Tables: Rounding types
 *    - Data Governance Framework: Domains, subject areas, authority types, coding schemes
 *    - IAM: Permissions and roles
 *    - System Configuration: Feature flags
 *
 * 2. Data Governance Layer
 *    - Dimensions: Quality classification (origin, nature, treatment)
 *    - Methodologies: Data sourcing and processing methods
 *    - Artefact Types: Types of data artefacts and their mappings
 *
 * 3. Data Catalogues Layer
 *    - Catalogs: Groupings of datasets by source/domain
 *    - Datasets: Data collection definitions with metadata
 *    - Dataset Dependencies: Relationships between datasets
 *    - Artefact Data: Actual reference data in staging tables
 *
 * NOTE: Production tables (countries, currencies, images) are NOT populated
 * here. Use the "Publish Datasets" feature in the Data Librarian window to
 * publish data from DQ artefact tables to production tables.
 *
 * Usage:
 *   psql -U ores_cli_user -d your_database -f populate/populate.sql
 */

-- Suppress noisy output during population
\timing off
\pset tuples_only on

\echo '=== Starting System Population ==='
\echo ''

-- =============================================================================
-- Data Governance Layer
-- =============================================================================

\echo '--- Data Governance Layer ---'
\ir governance/populate_governance.sql

-- =============================================================================
-- Data Catalogues Layer
-- =============================================================================

\echo ''
\echo '--- Data Catalogues Layer ---'
\ir catalogues/populate_catalogues.sql

\echo ''
\echo '=== System Population Complete ==='

-- Summary - restore normal output format
\pset tuples_only off

\echo ''
\echo '--- Overall Summary ---'

-- Foundation
select 'Foundation: Change Reason Categories' as entity, count(*) as count
from ores_dq_change_reason_categories_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Change Reasons', count(*)
from ores_dq_change_reasons_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Rounding Types', count(*)
from ores_refdata_rounding_types_tbl
union all
select 'Foundation: Data Domains', count(*)
from ores_dq_data_domains_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Subject Areas', count(*)
from ores_dq_subject_areas_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Coding Scheme Authority Types', count(*)
from ores_dq_coding_scheme_authority_types_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Coding Schemes', count(*)
from ores_dq_coding_schemes_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Permissions', count(*)
from ores_iam_permissions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Roles', count(*)
from ores_iam_roles_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: System Flags', count(*)
from ores_variability_feature_flags_tbl where name like 'system.%' and valid_to = ores_utility_infinity_timestamp_fn()
-- Governance
union all
select 'Governance: Origin Dimensions', count(*)
from ores_dq_origin_dimensions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Governance: Nature Dimensions', count(*)
from ores_dq_nature_dimensions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Governance: Treatment Dimensions', count(*)
from ores_dq_treatment_dimensions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Governance: Methodologies', count(*)
from ores_dq_methodologies_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Governance: Artefact Types', count(*)
from ores_dq_artefact_types_tbl
-- Catalogues
union all
select 'Catalogues: Catalogs', count(*)
from ores_dq_catalogs_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Catalogues: Datasets', count(*)
from ores_dq_datasets_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Catalogues: Dataset Dependencies', count(*)
from ores_dq_dataset_dependencies_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Catalogues: Artefact Images', count(*)
from ores_dq_images_artefact_tbl
union all
select 'Catalogues: Artefact Countries', count(*)
from ores_dq_countries_artefact_tbl
union all
select 'Catalogues: Artefact Currencies', count(*)
from ores_dq_currencies_artefact_tbl
union all
select 'Catalogues: Artefact IP Ranges', count(*)
from ores_dq_ip2country_artefact_tbl
order by entity;
