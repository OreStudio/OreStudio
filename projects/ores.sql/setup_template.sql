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
 * Template Database Setup
 *
 * Creates the ores_template database which serves as a blueprint for
 * creating new ORES database instances quickly.
 *
 * The template includes:
 *
 * 1. Schema Layer:
 *    - metadata schema: Data governance and classification (dq_*, change_control)
 *    - production schema: Operational data (refdata_*, iam_*, assets_*, etc.)
 *    - Utility functions in public schema
 *    - All tables with temporal/bitemporal support
 *    - Triggers and notification functions
 *
 * 2. Foundation Layer (essential lookup and configuration data):
 *    - Change Control: Categories and reasons for audit trail
 *    - Reference Data Lookup Tables: Rounding types
 *    - Data Governance Framework: Domains, subject areas, authority types, coding schemes
 *    - IAM: Permissions and roles
 *    - System Configuration: Feature flags
 *
 * USAGE:
 *   psql -U postgres -f setup_template.sql
 *
 * PREREQUISITES:
 *   - PostgreSQL superuser access
 *   - The 'ores' user must exist (run setup_user.sql first)
 *
 * After running this script, create new instances using:
 *   psql -U postgres -f create_instance.sql
 *
 * Or for a specific name:
 *   psql -U postgres -v db_name='my_database' -f create_instance.sql
 */

-- Drop template database if it exists (requires superuser)
-- Uncomment these lines to recreate the template:
-- update pg_database set datistemplate = false where datname = 'ores_template';
-- drop database if exists ores_template;

-- Create template database with clean template0 as base
create database ores_template with template = template0;

-- Grant permissions to ores user
grant all privileges on database ores_template to ores;

-- Connect to template database to create schema
\c ores_template

-- Install required extensions in the template database
-- (Extensions are per-database, so we need them here even if installed in postgres)
\ir ./setup_extensions.sql

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

-- NOTE: Whimsical names and database management functions are now in ores_admin.
-- See admin/setup_admin.sql for cluster-level utilities.

-- Create all tables, triggers, and functions
\ir ./create/create.sql

-- Populate foundation layer (essential lookup and configuration data)
\ir ./populate/foundation/populate_foundation.sql

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

-- NOTE: Instance-specific initialization (feature flags) is NOT included here.
-- Each instance created from this template should run instance/init_instance.sql

-- Mark as template database
-- This prevents TimescaleDB from starting background workers for the template,
-- which was causing issues with database creation.
\c postgres
update pg_database set datistemplate = true where datname = 'ores_template';

\echo ''
\echo '=========================================='
\echo 'Template database created successfully!'
\echo '=========================================='
\echo ''
\echo 'To create a new instance with a whimsical name:'
\echo '  psql -U postgres -f create_instance.sql'
\echo ''
\echo 'To create an instance with a specific name:'
\echo '  psql -U postgres -v db_name=my_database -f create_instance.sql'
\echo ''
