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
 * 1. RBAC: Permissions, roles, role-permission assignments
 * 2. System Flags: Bootstrap mode, user signups, etc.
 * 3. Flag Images: Country flags and placeholder images
 * 4. Currency-to-Flag Mappings: Links currencies to their flag images
 *
 * NOTE: Currencies themselves are imported separately via the CLI:
 *   ores.cli import currencies <file.xml>
 *
 * After importing currencies, re-run this script to assign flag mappings.
 *
 * Usage:
 *   psql -U ores -d your_database -f populate/populate.sql
 */

\echo '=== Starting System Population ==='
\echo ''

-- Change Control (must be populated before entities that use reasons)
\echo '--- Change Control ---'
\ir dq_change_reasons_populate.sql

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

-- Data Quality Methodologies
\echo ''
\echo '--- Data Quality Methodologies ---'
\ir dq_methodology_populate.sql

-- Data Quality Datasets
\echo ''
\echo '--- Data Quality Datasets ---'
\ir dq_dataset_populate.sql
\ir dq_images_artefact_populate.sql
\ir dq_tags_artefact_populate.sql
\ir dq_image_tags_artefact_populate.sql

-- RBAC (Role-Based Access Control)
\echo ''
\echo '--- RBAC Data ---'
\ir iam_permissions_populate.sql
\ir iam_roles_populate.sql

-- System Flags
\echo ''
\echo '--- System Flags ---'
\ir variability_system_flags_populate.sql

-- Flag Images
\echo ''
\echo '--- Flag Images ---'
\ir assets_load_flags.sql
\ir assets_flags_populate.sql

-- Currency-to-Flag Mappings
\echo ''
\echo '--- Currency Image Mappings ---'
\ir assets_currency_images_populate.sql

-- Countries
\echo ''
\echo '--- Countries ---'
\ir refdata_countries_populate.sql
\ir assets_country_images_populate.sql

\echo ''
\echo '=== System Population Complete ==='

-- Summary
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
select 'Currencies with Flags', count(*)
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
from ores.dq_origin_dimension_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Nature Dimensions', count(*)
from ores.dq_nature_dimension_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Treatment Dimensions', count(*)
from ores.dq_treatment_dimension_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Data Domains', count(*)
from ores.dq_data_domain_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Subject Areas', count(*)
from ores.dq_subject_area_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Data Quality Datasets', count(*)
from ores.dq_dataset_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;
