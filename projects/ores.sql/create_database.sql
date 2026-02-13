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
 * Database Creation (Postgres Superuser Phase)
 *
 * Creates a new empty ORES database with infrastructure grants. This script
 * handles operations that require superuser privileges:
 *   - CREATE DATABASE
 *   - GRANT ON DATABASE
 *   - CREATE EXTENSION
 *   - GRANT ON SCHEMA
 *
 * After running this script, run setup_schema.sql as ores_ddl_user to create
 * the schema and populate data.
 *
 * USAGE:
 *   psql -U postgres -v db_name='my_database' -f create_database.sql
 *
 *   -- Default name 'ores':
 *   psql -U postgres -f create_database.sql
 *
 * PREREQUISITES:
 *   - PostgreSQL superuser access
 *   - The ores roles/users must exist (run setup_user.sql first)
 *
 * NEXT STEPS:
 *   psql -U ores_ddl_user -d <db_name> -f setup_schema.sql
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
\echo ''

-- Create the database
create database :db_name;

-- Grant permissions to appropriate roles
grant all privileges on database :db_name to ores_owner;
grant connect, temp on database :db_name to ores_rw, ores_ro;

-- Connect to new database
\c :db_name

-- Install required extensions
create extension if not exists btree_gist;
create extension if not exists unaccent;

-- Grant schema permissions to appropriate roles
grant usage on schema public to ores_owner, ores_rw, ores_ro;
grant create on schema public to ores_owner;

\echo ''
\echo '=========================================='
\echo 'Database created successfully!'
\echo '=========================================='
\echo 'Database name:' :db_name
\echo ''
\echo 'Next step: Setup schema as DDL user'
\echo '  psql -U ores_ddl_user -d' :db_name '-f setup_schema.sql'
\echo ''
