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

-- Data Quality Framework (change control, dimensions, domains, subject areas, coding schemes)
\echo ''
\echo '--- Data Quality Framework ---'
\ir dq/populate_dq.sql

-- Reference Data Lookup Tables (must come before tables that reference them)
\echo ''
\echo '--- Reference Data Lookup Tables ---'
\ir refdata/refdata_rounding_types_populate.sql

-- Flag Icons Reference Data (Visual Assets catalog, datasets, and images - must come before ISO)
\echo ''
\echo '--- Flag Icons Reference Data ---'
\ir flags/populate_flags.sql

-- ISO Standards Reference Data (catalog, datasets, countries, currencies)
\echo ''
\echo '--- ISO Standards Reference Data ---'
\ir iso/populate_iso.sql

-- Solvaris Reference Data
\echo ''
\echo '--- Solvaris Reference Data ---'
\ir solvaris/populate_solvaris.sql

-- IP to Country Reference Data (iptoasn.com)
\echo ''
\echo '--- IP to Country Reference Data ---'
\ir ip2country/populate_ip2country.sql

-- FPML Reference Data (methodology, coding schemes, datasets, artefacts)
\echo ''
\echo '--- FPML Reference Data ---'
\ir fpml/populate_fpml.sql

-- Cryptocurrency Reference Data (datasets, images, currencies)
\echo ''
\echo '--- Cryptocurrency Reference Data ---'
\ir crypto/populate_crypto.sql

-- IAM (Identity and Access Management)
\echo ''
\echo '--- IAM ---'
\ir iam/populate_iam.sql

-- Variability (Feature Flags)
\echo ''
\echo '--- Variability ---'
\ir variability/populate_variability.sql

\echo ''
\echo '=== System Population Complete ==='

-- Summary - restore normal output format
\pset tuples_only off

\echo ''
\echo '--- Summary ---'

select 'Rounding Types' as entity, count(*) as count
from ores.refdata_rounding_types_tbl
union all
select 'Change Reasons', count(*)
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
