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
 * PostgreSQL Extensions Setup
 *
 * Installs all required PostgreSQL extensions for ORES. This script should be
 * run once per PostgreSQL cluster before creating the template database.
 *
 * PREREQUISITES:
 *   - PostgreSQL superuser access
 *   - TimescaleDB must be installed on the system:
 *       Debian/Ubuntu: apt install timescaledb-2-postgresql-16
 *       macOS:         brew install timescaledb
 *   - TimescaleDB must be added to shared_preload_libraries in postgresql.conf:
 *       shared_preload_libraries = 'timescaledb'
 *   - PostgreSQL must be restarted after modifying shared_preload_libraries
 *
 * USAGE:
 *   psql -U postgres -f setup_extensions.sql
 *
 * EXTENSIONS INSTALLED:
 *   - btree_gist: GiST index support for temporal exclusion constraints
 *   - timescaledb: Time-series database for session analytics
 *
 * NEXT STEPS:
 *   1. setup_user.sql       - Create application user
 *   2. admin/setup_admin.sql - Create admin database
 *   3. setup_template.sql    - Create template database
 *   4. create_instance.sql   - Create database instance
 */

\set ON_ERROR_STOP on

\echo ''
\echo 'Installing PostgreSQL extensions...'
\echo ''

-- btree_gist: Required for temporal exclusion constraints
-- This allows using GiST indexes with btree-indexable data types
create extension if not exists btree_gist;
\echo 'Installed: btree_gist'

-- TimescaleDB: Time-series database extension
-- Provides hypertables, compression, continuous aggregates, and retention policies
-- NOTE: Requires shared_preload_libraries = 'timescaledb' in postgresql.conf
create extension if not exists timescaledb;
\echo 'Installed: timescaledb'

\echo ''
\echo '=========================================='
\echo 'Extensions installed successfully!'
\echo '=========================================='
\echo ''
\echo 'TimescaleDB version:'
select extversion from pg_extension where extname = 'timescaledb';
\echo ''
\echo 'Next step: Create ORES user'
\echo '  psql -U postgres -v ores_password=''SECRET'' -f setup_user.sql'
\echo ''
