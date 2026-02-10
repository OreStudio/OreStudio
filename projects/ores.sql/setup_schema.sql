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
 * Schema Setup (DDL User Phase)
 *
 * Creates the schema, populates data, and grants table-level permissions.
 * This script runs as ores_ddl_user (member of ores_owner role) and handles
 * all operations that don't require superuser privileges.
 *
 * USAGE:
 *   psql -U ores_ddl_user -d <db_name> -f setup_schema.sql
 *
 *   -- With skip_validation (faster for development):
 *   psql -U ores_ddl_user -d <db_name> -v skip_validation='on' -f setup_schema.sql
 *
 * PREREQUISITES:
 *   - Database must already exist (run create_database.sql as postgres first)
 *   - Connected as ores_ddl_user to the target database
 */

\set ON_ERROR_STOP on

\pset pager off
\pset tuples_only on
\timing off

-- Handle skip_validation variable for seed function validation control
\if :{?skip_validation}
    select set_config('ores.skip_validation', :'skip_validation', false);
\else
    select set_config('ores.skip_validation', 'off', false);
\endif

\echo ''
\echo 'Setting up schema...'
\echo ''

-- Create all tables, triggers, and functions
\ir ./create/create.sql

-- Populate foundation layer (essential lookup and configuration data)
\ir ./populate/foundation/foundation_populate.sql

-- Populate governance and catalogues layers (dimensions, methodologies,
-- catalogs, datasets, dataset bundles, etc.)
\ir ./populate/populate.sql

-- Grant table permissions to appropriate roles
-- Note: TRUNCATE is included for test database cleanup
-- Owner role gets full access
grant select, insert, update, delete, truncate on all tables in schema public to ores_owner;

-- RW role gets standard DML access
grant select, insert, update, delete, truncate on all tables in schema public to ores_rw;

-- RO role gets read-only access
grant select on all tables in schema public to ores_ro;

-- Grant sequence permissions to appropriate roles
grant usage, select on all sequences in schema public to ores_owner, ores_rw;

-- Set default privileges for any future tables created by ores_ddl_user
alter default privileges in schema public
    grant select, insert, update, delete, truncate on tables to ores_rw;

alter default privileges in schema public
    grant select on tables to ores_ro;

alter default privileges in schema public
    grant usage, select on sequences to ores_rw;

-- Initialize instance-specific feature flags
\ir ./instance/init_instance.sql

\echo ''
\echo '=========================================='
\echo 'Schema setup complete!'
\echo '=========================================='
\echo ''
