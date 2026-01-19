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
 * 4. Production Data: Images, countries, currencies (via DQ population functions)
 * 5. RBAC: Permissions, roles, role-permission assignments
 * 6. System Flags: Bootstrap mode, user signups, etc.
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

-- Data Quality Catalog Dependencies
\echo ''
\echo '--- Data Quality Catalog Dependencies ---'
\ir dq_catalog_dependency_populate.sql

-- Data Quality Artefacts
\ir dq_flags_images_artefact_populate.sql
\ir dq_crypto_images_artefact_populate.sql
\ir dq_countries_artefact_populate.sql
\ir dq_currencies_artefact_populate.sql
\ir dq_fpml_non_iso_currencies_artefact_populate.sql
\ir dq_cryptocurrencies_artefact_populate.sql

-- RBAC (Role-Based Access Control)
\echo ''
\echo '--- RBAC Data ---'
\ir iam_permissions_populate.sql
\ir iam_roles_populate.sql

-- System Flags
\echo ''
\echo '--- System Flags ---'
\ir variability_system_flags_populate.sql

-- Production Data (populated from DQ staging tables)
\echo ''
\echo '--- Production Data (from DQ) ---'
\ir dq_populate_production.sql

\echo ''
\echo '=== System Population Complete ==='

-- Summary - restore normal output format
\pset tuples_only off

\echo ''
\echo '--- Summary ---'

select 'Change Reasons' as entity, count(*) as count
from ores.dq_change_reasons_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Countries', count(*)
from ores.refdata_countries_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Countries with Flags', count(*)
from ores.refdata_countries_tbl where image_id is not null and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (fiat.major)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'fiat.major' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (fiat.emerging)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'fiat.emerging' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (commodity)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'commodity' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (supranational)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'supranational' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (fiat.offshore)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'fiat.offshore' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (fiat.historical)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'fiat.historical' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (crypto.major)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'crypto.major' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies with Images', count(*)
from ores.refdata_currencies_tbl where image_id is not null and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Flag Images', count(*)
from ores.assets_images_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Permissions', count(*)
from ores.iam_permissions_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Change Reason Categories', count(*)
from ores.dq_change_reason_categories_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Roles', count(*)
from ores.iam_roles_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'System Flags', count(*)
from ores.variability_feature_flags_tbl where name like 'system.%' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Origin Dimensions', count(*)
from ores.dq_origin_dimensions_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Nature Dimensions', count(*)
from ores.dq_nature_dimensions_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Treatment Dimensions', count(*)
from ores.dq_treatment_dimensions_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Catalogs', count(*)
from ores.dq_catalogs_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Data Domains', count(*)
from ores.dq_data_domains_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Subject Areas', count(*)
from ores.dq_subject_areas_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'DQ Coding Scheme Authority Types', count(*)
from ores.dq_coding_scheme_authority_types_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Coding Schemes', count(*)
from ores.dq_coding_schemes_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Datasets', count(*)
from ores.dq_datasets_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Images', count(*)
from ores.dq_images_artefact_tbl
union all
select 'Data Quality Countries', count(*)
from ores.dq_countries_artefact_tbl
union all
select 'Data Quality Currencies', count(*)
from ores.dq_currencies_artefact_tbl
order by entity;
