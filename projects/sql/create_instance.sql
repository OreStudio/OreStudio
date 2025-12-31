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
 * Create Instance from Template
 *
 * Creates a new ORES database instance from the ores_template.
 * This is FAST because PostgreSQL copies the template at the filesystem level.
 *
 * USAGE:
 *   -- With auto-generated whimsical name:
 *   psql -U postgres -f create_instance.sql
 *
 *   -- With specific name:
 *   psql -U postgres -v db_name='my_database' -f create_instance.sql
 *
 * PREREQUISITES:
 *   - The ores_admin database must exist (run admin/setup_admin.sql first)
 *   - The ores_template database must exist (run setup_template.sql first)
 *   - The 'ores' user must exist (run setup_user.sql first)
 *
 * OUTPUT:
 *   Creates a fully-configured database with all tables, functions, and
 *   reference data. Instance-specific feature flags are initialized.
 *
 * NOTE: When using auto-generated names, there is a small race condition
 * window between name generation and database creation. If running multiple
 * instances of this script concurrently, consider using explicit db_name
 * parameters to avoid potential conflicts.
 */

\set ON_ERROR_STOP on

-- Check if db_name was provided, if not generate a whimsical name
\if :{?db_name}
    -- User provided a name, use it
    \echo 'Using provided database name...'
\else
    -- Generate a whimsical name from ores_admin
    \echo 'Generating whimsical database name...'

    -- Connect to admin database to use the whimsical name function
    \c ores_admin

    -- Generate and store the name
    select generate_unique_database_name_from_server() as generated_name \gset

    -- Set db_name to the generated value
    \set db_name :generated_name

    -- Back to postgres database to create the new one
    \c postgres
\endif

\echo ''
\echo 'Creating database:' :db_name
\echo ''

-- Create the database from template
create database :db_name with template = ores_template;

-- Grant permissions to ores user
grant all privileges on database :db_name to ores;

-- Connect to new database and initialize instance-specific data
\c :db_name

-- Initialize instance-specific feature flags
\ir ./instance/init_instance.sql

\echo ''
\echo '=========================================='
\echo 'Database created successfully!'
\echo '=========================================='
\echo 'Database name:' :db_name
\echo ''
\echo 'Connect with:'
\echo '  psql -U ores -d' :db_name
\echo ''
