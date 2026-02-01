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
 *   - The ores roles/users must exist (run setup_user.sql first)
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

-- Grant permissions to appropriate roles
grant all privileges on database :db_name to ores_owner;
grant connect, temp on database :db_name to ores_rw, ores_ro;

-- Connect to new database
\c :db_name

-- Create schemas
create schema if not exists metadata;
create schema if not exists production;
create extension if not exists btree_gist;

-- Grant schema permissions to appropriate roles
grant usage on schema metadata to ores_owner, ores_rw, ores_ro;
grant usage on schema production to ores_owner, ores_rw, ores_ro;
grant create on schema metadata to ores_owner;
grant create on schema production to ores_owner;

-- Set default search_path for all ores users
-- Note: search_path must be set on users, not group roles (it doesn't inherit)
alter role ores_ddl_user set search_path to production, metadata, public;
alter role ores_cli_user set search_path to production, metadata, public;
alter role ores_wt_user set search_path to production, metadata, public;
alter role ores_comms_user set search_path to production, metadata, public;
alter role ores_http_user set search_path to production, metadata, public;
alter role ores_test_ddl_user set search_path to production, metadata, public;
alter role ores_test_dml_user set search_path to production, metadata, public;
alter role ores_readonly_user set search_path to production, metadata, public;

-- Create all tables, triggers, and functions
\ir ./create/create.sql

-- Populate foundation layer (essential lookup and configuration data)
-- This is normally included in the template, but since we're creating
-- directly without a template, we need to include it here.
\ir ./populate/foundation/foundation_populate.sql

-- Populate governance and catalogues layers (dimensions, methodologies,
-- catalogs, datasets, dataset bundles, etc.)
\ir ./populate/populate.sql

-- Grant table permissions to appropriate roles
-- Note: TRUNCATE is included for test database cleanup
-- Owner role gets full access
grant select, insert, update, delete, truncate on all tables in schema public to ores_owner;
grant select, insert, update, delete, truncate on all tables in schema metadata to ores_owner;
grant select, insert, update, delete, truncate on all tables in schema production to ores_owner;

-- RW role gets standard DML access
grant select, insert, update, delete, truncate on all tables in schema public to ores_rw;
grant select, insert, update, delete, truncate on all tables in schema metadata to ores_rw;
grant select, insert, update, delete, truncate on all tables in schema production to ores_rw;

-- RO role gets read-only access
grant select on all tables in schema public to ores_ro;
grant select on all tables in schema metadata to ores_ro;
grant select on all tables in schema production to ores_ro;

-- Grant sequence permissions to appropriate roles
grant usage, select on all sequences in schema public to ores_owner, ores_rw;
grant usage, select on all sequences in schema metadata to ores_owner, ores_rw;
grant usage, select on all sequences in schema production to ores_owner, ores_rw;

-- Set default privileges for any future tables
-- For DDL user (owner role) - ensure new objects are accessible by other roles
alter default privileges for role ores_owner in schema metadata
    grant select, insert, update, delete, truncate on tables to ores_rw;

alter default privileges for role ores_owner in schema production
    grant select, insert, update, delete, truncate on tables to ores_rw;

alter default privileges for role ores_owner in schema metadata
    grant select on tables to ores_ro;

alter default privileges for role ores_owner in schema production
    grant select on tables to ores_ro;

alter default privileges for role ores_owner in schema metadata
    grant usage, select on sequences to ores_rw;

alter default privileges for role ores_owner in schema production
    grant usage, select on sequences to ores_rw;

-- Initialize instance-specific feature flags
\ir ./instance/init_instance.sql

\echo ''
\echo '=========================================='
\echo 'Database created successfully!'
\echo '=========================================='
\echo 'Database name:' :db_name
\echo ''
\echo 'Connect with:'
\echo '  psql -U ores_cli_user -d' :db_name
\echo ''
