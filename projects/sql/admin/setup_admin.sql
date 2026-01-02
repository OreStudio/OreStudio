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
 *   - The 'ores' user must exist (run setup_user.sql first)
 *
 * This database does NOT contain application code - only admin utilities.
 * Application schema is in ores_template.
 */

\set ON_ERROR_STOP on

-- Drop admin database if it exists (for recreation)
-- Uncomment these lines to recreate:
-- DROP DATABASE IF EXISTS ores_admin;

-- Create admin database
CREATE DATABASE ores_admin;

-- Grant permissions to ores user
GRANT ALL PRIVILEGES ON DATABASE ores_admin TO ores;

-- Connect to admin database to create utilities
\c ores_admin

\echo ''
\echo 'Loading admin utilities...'
\echo ''

-- Load admin functions
\ir whimsical_names.sql
\ir database_functions.sql
\ir cleanup_functions.sql

-- Grant schema object permissions to ores user
-- (Database-level grants don't include schema object access)
GRANT USAGE ON SCHEMA public TO ores;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO ores;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO ores;

-- Set default privileges for future objects
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO ores;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT EXECUTE ON FUNCTIONS TO ores;

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
\echo '  SELECT * FROM ores_databases;'
\echo ''
\echo 'To create a new instance:'
\echo '  SELECT generate_unique_database_name_from_server();'
\echo ''
