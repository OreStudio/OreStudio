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
 * Seeds the database with essential system data required for the
 * application to function. All scripts are idempotent and can be
 * safely re-run without creating duplicate data.
 *
 * This script is run automatically during template creation and seeds:
 * 1. Change Control: Change reasons and categories
 * 2. Data Quality: Dimensions, domains, subject areas, methodologies, datasets
 * 3. DQ Artefacts: Images, countries, currencies (staging data)
 * 4. RBAC: Permissions, roles, role-permission assignments
 * 5. System Flags: Bootstrap mode, user signups, etc.
 *
 * NOTE: Production tables (countries, currencies, images) are NOT populated
 * here. Use the "Publish Datasets" feature in the Data Librarian window to
 * publish data from DQ artefact tables to production tables.
 *
 * Usage:
 *   psql -U ores -d your_database -f populate/populate.sql
 */

-- Suppress noisy output during population
\timing off
\pset tuples_only on

\echo '=== Starting System Population ==='
\echo ''

-- Change Control (must be populated before entities that use reasons)
\echo '--- Change Control ---'
\ir dq_change_reasons_populate.sql

-- Data Quality Catalogs
\echo ''
\echo '--- Data Quality Catalogs ---'
\ir dq_catalog_populate.sql

-- Data Quality Data Domains
\echo ''
\echo '--- Data Quality Data Domains ---'
\ir dq_data_domain_populate.sql

-- Data Quality Dimensions
\echo ''
\echo '--- Data Quality Dimensions ---'
\ir dq_origin_dimension_populate.sql
\ir dq_nature_dimension_populate.sql
\ir dq_treatment_dimension_populate.sql

-- Data Quality Subject Areas
\echo ''
\echo '--- Data Quality Subject Areas ---'
\ir dq_subject_area_populate.sql

-- Data Quality Coding Scheme Authority Types
\echo ''
\echo '--- Data Quality Coding Scheme Authority Types ---'
\ir dq_coding_scheme_authority_type_populate.sql

-- Data Quality Coding Schemes
\echo ''
\echo '--- Data Quality Coding Schemes ---'
\ir dq_coding_scheme_populate.sql

-- Data Quality Methodologies
\echo ''
\echo '--- Data Quality Methodologies ---'
\ir dq_methodology_populate.sql

-- Data Quality Datasets
\echo ''
\echo '--- Data Quality Datasets ---'
\ir dq_dataset_populate.sql

-- Data Quality Dataset Tags
\echo ''
\echo '--- Data Quality Dataset Tags ---'
\ir dq_dataset_tag_populate.sql

-- Data Quality Dataset Dependencies
\echo ''
\echo '--- Data Quality Dataset Dependencies ---'
\ir dq_dataset_dependency_populate.sql

-- Data Quality Artefacts
\ir dq_flags_images_artefact_populate.sql
\ir dq_countries_artefact_populate.sql
\ir dq_currencies_artefact_populate.sql
\ir dq_ip2country_artefact_populate.sql
\ir solvaris_batch_execute.sql

-- FPML Reference Data (methodology, coding schemes, datasets, artefacts)
\echo ''
\echo '--- FPML Reference Data ---'
\ir fpml/fpml.sql

-- Cryptocurrency Reference Data (datasets, images, currencies)
\echo ''
\echo '--- Cryptocurrency Reference Data ---'
\ir crypto/crypto.sql

-- RBAC (Role-Based Access Control)
\echo ''
\echo '--- RBAC Data ---'
\ir iam_permissions_populate.sql
\ir iam_roles_populate.sql

-- System Flags
\echo ''
\echo '--- System Flags ---'
\ir variability_system_flags_populate.sql

\echo ''
\echo '=== System Population Complete ==='

-- Summary - restore normal output format
\pset tuples_only off

\echo ''
\echo '--- Summary ---'

select 'Change Reasons' as entity, count(*) as count
from ores.dq_change_reasons_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Change Reason Categories', count(*)
from ores.dq_change_reason_categories_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Permissions', count(*)
from ores.iam_permissions_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Roles', count(*)
from ores.iam_roles_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'System Flags', count(*)
from ores.variability_feature_flags_tbl where name like 'system.%' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Origin Dimensions', count(*)
from ores.dq_origin_dimensions_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Nature Dimensions', count(*)
from ores.dq_nature_dimensions_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Treatment Dimensions', count(*)
from ores.dq_treatment_dimensions_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Catalogs', count(*)
from ores.dq_catalogs_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Data Domains', count(*)
from ores.dq_data_domains_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Subject Areas', count(*)
from ores.dq_subject_areas_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Coding Scheme Authority Types', count(*)
from ores.dq_coding_scheme_authority_types_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Coding Schemes', count(*)
from ores.dq_coding_schemes_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Datasets', count(*)
from ores.dq_datasets_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Dataset Dependencies', count(*)
from ores.dq_dataset_dependencies_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Artefact: Images', count(*)
from ores.dq_images_artefact_tbl
union all
select 'DQ Artefact: Countries', count(*)
from ores.dq_countries_artefact_tbl
union all
select 'DQ Artefact: Currencies', count(*)
from ores.dq_currencies_artefact_tbl
union all
select 'DQ Artefact: IP Ranges', count(*)
from ores.dq_ip2country_artefact_tbl
union all
select 'DQ Artefact: Account Types', count(*)
from ores.dq_account_types_artefact_tbl
union all
select 'DQ Artefact: Asset Classes', count(*)
from ores.dq_asset_classes_artefact_tbl
union all
select 'DQ Artefact: Asset Measures', count(*)
from ores.dq_asset_measures_artefact_tbl
union all
select 'DQ Artefact: Benchmark Rates', count(*)
from ores.dq_benchmark_rates_artefact_tbl
union all
select 'DQ Artefact: Business Centres', count(*)
from ores.dq_business_centres_artefact_tbl
union all
select 'DQ Artefact: Business Processes', count(*)
from ores.dq_business_processes_artefact_tbl
union all
select 'DQ Artefact: Cashflow Types', count(*)
from ores.dq_cashflow_types_artefact_tbl
union all
select 'DQ Artefact: Entity Classifications', count(*)
from ores.dq_entity_classifications_artefact_tbl
union all
select 'DQ Artefact: Local Jurisdictions', count(*)
from ores.dq_local_jurisdictions_artefact_tbl
union all
select 'DQ Artefact: Party Relationships', count(*)
from ores.dq_party_relationships_artefact_tbl
union all
select 'DQ Artefact: Party Roles', count(*)
from ores.dq_party_roles_artefact_tbl
union all
select 'DQ Artefact: Person Roles', count(*)
from ores.dq_person_roles_artefact_tbl
union all
select 'DQ Artefact: Regulatory Corporate Sectors', count(*)
from ores.dq_regulatory_corporate_sectors_artefact_tbl
union all
select 'DQ Artefact: Reporting Regimes', count(*)
from ores.dq_reporting_regimes_artefact_tbl
union all
select 'DQ Artefact: Supervisory Bodies', count(*)
from ores.dq_supervisory_bodies_artefact_tbl
order by entity;
