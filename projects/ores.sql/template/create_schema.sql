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
 * Template Schema Creation Script
 *
 * Creates the complete ORES database schema including:
 * - Schema and extensions
 * - Utility functions
 * - All tables with temporal/bitemporal support
 * - Triggers and notification functions
 * - Reference data (currencies, flags)
 * - RBAC seed data (permissions, roles)
 * - System flags (bootstrap_mode, user_signups, etc.)
 *
 * This script is used by both:
 * - setup_template.sql (to create the template database)
 * - create_database_direct.sql (to create a standalone database)
 */

-- Create schema and extensions
create schema if not exists ores;
create extension if not exists btree_gist;

-- Grant schema permissions to ores user
grant usage on schema ores to ores;
grant create on schema ores to ores;

-- Utility functions
\ir ../schema/utility_functions_create.sql

-- NOTE: Whimsical names and database management functions are now in ores_admin.
-- See admin/setup_admin.sql for cluster-level utilities.

-- Change control tables (must be created before entities that reference them)
\ir ../schema/dq_change_reason_categories_create.sql
\ir ../schema/dq_change_reason_categories_notify_trigger.sql
\ir ../schema/dq_change_reasons_create.sql
\ir ../schema/dq_change_reasons_notify_trigger.sql
\ir ../schema/dq_change_reason_functions_create.sql

-- Reference data tables
\ir ../schema/refdata_currencies_create.sql
\ir ../schema/refdata_currencies_notify_trigger.sql
\ir ../schema/refdata_countries_create.sql
\ir ../schema/refdata_countries_notify_trigger.sql

-- IAM tables
\ir ../schema/iam_accounts_create.sql
\ir ../schema/iam_accounts_notify_trigger.sql
\ir ../schema/iam_login_info_create.sql
\ir ../schema/iam_sessions_create.sql
\ir ../schema/iam_session_stats_create.sql
\ir ../schema/iam_permissions_create.sql
\ir ../schema/iam_permissions_notify_trigger.sql
\ir ../schema/iam_roles_create.sql
\ir ../schema/iam_roles_notify_trigger.sql
\ir ../schema/iam_role_permissions_create.sql
\ir ../schema/iam_account_roles_create.sql
\ir ../schema/iam_rbac_functions_create.sql

-- Variability tables
\ir ../schema/variability_feature_flags_create.sql
\ir ../schema/variability_feature_flags_notify_trigger.sql

-- Telemetry tables
\ir ../schema/telemetry_logs_create.sql
\ir ../schema/telemetry_stats_functions_create.sql

-- Asset tables
\ir ../schema/assets_images_create.sql
\ir ../schema/assets_tags_create.sql
\ir ../schema/assets_image_tags_create.sql

-- Geo tables and functions
\ir ../schema/geo_ip2country_create.sql

-- Data Quality tables
\ir ../schema/dq_catalog_create.sql
\ir ../schema/dq_catalog_dependency_create.sql
\ir ../schema/dq_data_domain_create.sql
\ir ../schema/dq_subject_area_create.sql
\ir ../schema/dq_coding_scheme_authority_type_create.sql
\ir ../schema/dq_coding_scheme_create.sql
\ir ../schema/dq_origin_dimension_create.sql
\ir ../schema/dq_nature_dimension_create.sql
\ir ../schema/dq_treatment_dimension_create.sql
\ir ../schema/dq_methodology_create.sql
\ir ../schema/dq_dataset_create.sql
\ir ../schema/dq_countries_artefact_create.sql
\ir ../schema/dq_currencies_artefact_create.sql
\ir ../schema/dq_images_artefact_create.sql
\ir ../schema/dq_tags_artefact_create.sql
\ir ../schema/dq_image_tags_artefact_create.sql
\ir ../schema/dq_functions_create.sql
\ir ../schema/dq_population_functions_create.sql

-- NOTE: The template database contains schema only, no data.
-- To seed data after creating an instance:
--   psql -U ores -d your_database -f populate/populate.sql        # RBAC + system flags
--   psql -U ores -d your_database -f populate/reference_data.sql  # Currencies, flags, images

-- Grant table permissions to ores user
-- Note: TRUNCATE is included for test database cleanup
grant select, insert, update, delete, truncate on all tables in schema ores to ores;
grant usage, select on all sequences in schema ores to ores;

-- Set default privileges for any future tables
alter default privileges in schema ores grant select, insert, update, delete, truncate on tables to ores;
alter default privileges in schema ores grant usage, select on sequences to ores;
