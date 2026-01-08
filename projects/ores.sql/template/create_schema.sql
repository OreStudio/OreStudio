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

-- Core tables
\ir ../schema/currencies_create.sql
\ir ../schema/currencies_notify_trigger.sql
\ir ../schema/countries_create.sql
\ir ../schema/countries_notify_trigger.sql
\ir ../schema/accounts_create.sql
\ir ../schema/accounts_notify_trigger.sql
\ir ../schema/feature_flags_create.sql
\ir ../schema/feature_flags_notify_trigger.sql
\ir ../schema/login_info_create.sql
\ir ../schema/sessions_create.sql
\ir ../schema/session_stats_create.sql

-- Telemetry tables
\ir ../schema/telemetry_logs_create.sql
\ir ../schema/telemetry_stats_create.sql

-- RBAC tables
\ir ../schema/permissions_create.sql
\ir ../schema/roles_create.sql
\ir ../schema/role_permissions_create.sql
\ir ../schema/account_roles_create.sql
\ir ../schema/rbac_functions_create.sql

-- Image/asset tables
\ir ../schema/images_create.sql
\ir ../schema/tags_create.sql
\ir ../schema/image_tags_create.sql
\ir ../schema/images_functions_create.sql

-- Geolocation tables and functions
\ir ../schema/geolocation_create.sql

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
