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
 * Admin Database Setup
 *
 * Creates the ores_admin database which contains cluster-level utilities:
 * - Database cleanup views and functions
 * - Whimsical name generation for new instances
 * - Database management helpers
 *
 * USAGE:
 *   psql -U postgres -f admin/setup_admin.sql
 *
 * PREREQUISITES:
 *   - PostgreSQL superuser access
 *   - The ores roles/users must exist (run setup_user.sql first)
 *
 * This database does NOT contain application code - only admin utilities.
 * Application schema is in ores_template.
 */

\set ON_ERROR_STOP on

-- Drop admin database if it exists (for recreation)
-- Uncomment these lines to recreate:
-- DROP DATABASE IF EXISTS ores_admin;

-- Create admin database
create database ores_admin;

-- Grant permissions to appropriate roles
grant all privileges on database ores_admin to ores_owner;
grant connect, temp on database ores_admin to ores_rw, ores_ro;

-- Connect to admin database to create utilities
\c ores_admin

\echo ''
\echo 'Loading admin utilities...'
\echo ''

-- Load admin functions
\ir admin_whimsical_names_create.sql
\ir admin_database_functions_create.sql
\ir admin_cleanup_functions_create.sql

-- Grant schema object permissions to appropriate roles
-- (Database-level grants don't include schema object access)
grant usage on schema public to ores_owner, ores_rw, ores_ro;
grant select on all tables in schema public to ores_owner, ores_rw, ores_ro;
grant execute on all functions in schema public to ores_owner, ores_rw, ores_ro;

-- Set default privileges for future objects
alter default privileges in schema public grant select on tables to ores_owner, ores_rw, ores_ro;
alter default privileges in schema public grant execute on functions to ores_owner, ores_rw;

\echo ''
\echo '=========================================='
\echo 'Admin database created successfully!'
\echo '=========================================='
\echo ''
\echo 'ores_admin contains:'
\echo '  - Whimsical name generation functions'
\echo '  - Database listing and creation helpers'
\echo '  - Cleanup views and functions'
\echo ''
\echo 'To list all ORES databases:'
\echo '  \c ores_admin'
\echo '  SELECT * FROM admin_ores_databases_view;'
\echo ''
\echo 'To create a new instance:'
\echo '  SELECT admin_generate_unique_database_name_from_server_fn();'
\echo ''
