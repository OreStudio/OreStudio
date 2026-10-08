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
 * This script runs as the DDL user (member of the owner role) and handles
 * all operations that don't require superuser privileges.
 *
 * USAGE:
 *   psql -U <ddl_user> -d <db_name> \
 *     -v owner_role=<owner_role> -v rw_role=<rw_role> -v ro_role=<ro_role> \
 *     -v ddl_user=<ddl_user> ... \
 *     -f setup_schema.sql
 *
 *   -- With skip_validation (faster for development):
 *   psql ... -v skip_validation='on' -f setup_schema.sql
 *
 * PREREQUISITES:
 *   - Database must already exist (run create_database.sql as postgres first)
 *   - Connected as the DDL user to the target database
 *   - Role name variables must be passed via -v flags (see setup_database.sh)
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
grant select, insert, update, delete, truncate on all tables in schema public to :owner_role;

-- RW role gets standard DML access
grant select, insert, update, delete, truncate on all tables in schema public to :rw_role;

-- RO role gets read-only access
grant select on all tables in schema public to :ro_role;

-- Grant sequence permissions to appropriate roles
grant usage, select on all sequences in schema public to :owner_role, :rw_role;

-- Set default privileges for any future tables created by the DDL user
alter default privileges in schema public
    grant select, insert, update, delete, truncate on tables to :rw_role;

alter default privileges in schema public
    grant select on tables to :ro_role;

alter default privileges in schema public
    grant usage, select on sequences to :rw_role;

-- Grant per-service least-privilege table access
\ir ./create/iam/iam_service_db_grants_create.sql

-- The test users drive the services directly, so the function a service reads a
-- system-tenant template through is granted to them too. The production roles
-- do not get it: only the services that copy templates, and the tests that
-- stand in for them, may read across the tenant boundary.
grant execute on function ores_assets_get_template_image_fn(text)
    to :test_dml_user, :test_ddl_user;

-- The variability service reads a tenant's settings through a SECURITY DEFINER
-- function, because its service context holds no direct SELECT grant on the
-- settings table. The test users stand in for that service, so the function is
-- granted to them too; the production roles do not get it, because it takes a
-- tenant as a parameter and reads across the tenant boundary.
grant execute on function ores_variability_get_system_settings_fn(uuid, uuid)
    to :test_dml_user, :test_ddl_user;

-- No writer needs this to write a setting: the settings trigger is SECURITY
-- DEFINER, so it resolves the party as its owner whatever role did the insert.
-- The grant stays for the two invoker-rights seed functions that call it
-- directly, ores_variability_system_settings_upsert_fn and
-- ores_variability_system_settings_set_fn, which the test users drive. The
-- production service roles still do not get it: it takes a tenant as a
-- parameter and reads across the tenant boundary.
grant execute on function ores_variability_resolve_system_party_fn(uuid)
    to :test_dml_user, :test_ddl_user;

-- Initialize instance-specific feature flags
\ir ./instance/init_instance.sql

\echo ''
\echo '=========================================='
\echo 'Schema setup complete!'
\echo '=========================================='
\echo ''
