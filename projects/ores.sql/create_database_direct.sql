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
 * Direct Database Creation (No Template Required)
 *
 * Creates a new ORES database from scratch without using a template.
 * This is useful for environments where templates are not available
 * or not desired.
 *
 * NOTE: This is SLOWER than create_instance.sql because it runs all
 * schema creation scripts instead of copying a template.
 *
 * USAGE:
 *   -- With specific name (required for direct creation):
 *   psql -U postgres -v db_name='my_database' -f create_database_direct.sql
 *
 *   -- Default name 'ores':
 *   psql -U postgres -f create_database_direct.sql
 *
 * PREREQUISITES:
 *   - PostgreSQL superuser access
 *   - The 'ores' user must exist (run setup_user.sql first)
 *
 * OUTPUT:
 *   Creates a fully-configured database with all tables, functions,
 *   reference data, and instance-specific feature flags.
 */

\set ON_ERROR_STOP on

-- Use provided name or default to 'ores'
\if :{?db_name}
    \echo 'Using provided database name...'
\else
    \set db_name 'ores'
    \echo 'Using default database name: ores'
\endif

\echo ''
\echo 'Creating database:' :db_name
\echo '(This may take a moment as all schemas are created from scratch)'
\echo ''

-- Create the database
create database :db_name;

-- Grant permissions to ores user
grant all privileges on database :db_name to ores;

-- Connect to new database
\c :db_name

-- Create schemas
create schema if not exists metadata;
create schema if not exists production;
create extension if not exists btree_gist;

-- Grant schema permissions to ores user
grant usage on schema metadata to ores;
grant usage on schema production to ores;
grant create on schema metadata to ores;
grant create on schema production to ores;

-- Set search path for convenience
alter role ores set search_path to production, metadata, public;

-- Create all tables, triggers, and functions
\ir ./create/create.sql

-- Grant table permissions to ores user
-- Note: TRUNCATE is included for test database cleanup
grant select, insert, update, delete, truncate on all tables in schema metadata to ores;
grant select, insert, update, delete, truncate on all tables in schema production to ores;
grant usage, select on all sequences in schema metadata to ores;
grant usage, select on all sequences in schema production to ores;

-- Set default privileges for any future tables
alter default privileges in schema metadata grant select, insert, update, delete, truncate on tables to ores;
alter default privileges in schema metadata grant usage, select on sequences to ores;
alter default privileges in schema production grant select, insert, update, delete, truncate on tables to ores;
alter default privileges in schema production grant usage, select on sequences to ores;

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
