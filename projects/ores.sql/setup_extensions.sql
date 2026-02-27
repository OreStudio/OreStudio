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
 * Installs required PostgreSQL extensions for ORES. This script is used in
 * two contexts:
 *   1. Run manually against postgres database to verify extensions are available
 *   2. Included by create_database.sql to install extensions in new databases
 *
 * Extensions are per-database in PostgreSQL, so they must be installed in
 * each database that needs them.
 *
 * NOTE: pg_cron is NOT installed here. It must live in the postgres database
 * (the cron.database_name). See setup_extensions_postgres.sql.
 *
 * REQUIRED EXTENSIONS:
 *   - btree_gist: GiST index support for temporal exclusion constraints
 *   - unaccent: Accent-insensitive text search for normalised name generation
 *
 * OPTIONAL EXTENSIONS:
 *   - pgtap: Unit testing framework for pgTAP SQL tests (recommended)
 *   - pgmq: Lightweight message queue for async workflows (recommended)
 *     If not available, message-queue-based features will not function.
 *   - timescaledb: Time-series database for session analytics (recommended)
 *     If not available, sessions table will use regular PostgreSQL tables.
 *
 * PREREQUISITES FOR PGMQ (optional):
 *   - pgmq must be installed on the system:
 *       Debian/Ubuntu: apt install postgresql-NN-pgmq
 *       (or via PGXN / build from source)
 *
 * PREREQUISITES FOR TIMESCALEDB (optional):
 *   - TimescaleDB must be installed on the system:
 *       Debian/Ubuntu: apt install timescaledb-2-postgresql-16
 *       macOS:         brew install timescaledb
 *   - TimescaleDB must be added to shared_preload_libraries in postgresql.conf:
 *       shared_preload_libraries = 'timescaledb'
 *   - PostgreSQL must be restarted after modifying shared_preload_libraries
 *
 * TIMESCALEDB LICENSE:
 *   Some features (compression, retention policies, continuous aggregates)
 *   require the "Timescale License" (community edition), not Apache-only.
 *   To enable: ALTER SYSTEM SET timescaledb.license = 'timescale';
 *              SELECT pg_reload_conf();
 *   The scripts will detect the license and skip unsupported features.
 *
 * USAGE:
 *   psql -U postgres -f setup_extensions.sql
 *
 * NEXT STEPS:
 *   1. setup_user.sql       - Create roles and users
 *   2. create_database.sql   - Create database (postgres)
 *   3. setup_schema.sql      - Setup schema (ores_ddl_user)
 */

\set ON_ERROR_STOP on

\echo ''
\echo 'Installing PostgreSQL extensions...'
\echo ''

-- btree_gist: Required for temporal exclusion constraints
-- This allows using GiST indexes with btree-indexable data types
create extension if not exists btree_gist;
\echo 'Installed: btree_gist'

-- unaccent: Required for accent-insensitive text search (e.g. normalised_name)
create extension if not exists unaccent;
\echo 'Installed: unaccent'

-- pgTAP: Unit testing framework (OPTIONAL)
-- Provides functions for writing SQL-level unit tests (plan, is, ok, etc.)
-- If not available, pgTAP SQL tests cannot be run.
do $$
declare
    pgtap_available boolean;
begin
    select exists (
        select 1 from pg_available_extensions where name = 'pgtap'
    ) into pgtap_available;

    if pgtap_available then
        create extension if not exists pgtap;
        raise notice 'Installed: pgtap';
    else
        raise notice 'pgTAP not available - SQL unit tests will not be runnable';
        raise notice '(Install with: apt install postgresql-NN-pgtap)';
    end if;
end $$;

-- pgmq: Lightweight message queue extension (OPTIONAL)
-- Provides pgmq.create(), pgmq.send(), pgmq.read(), pgmq.delete(), etc.
-- Required for async message-queue-based workflows.
-- If not available, queue-based features will not function.
do $$
declare
    pgmq_available boolean;
begin
    select exists (
        select 1 from pg_available_extensions where name = 'pgmq'
    ) into pgmq_available;

    if pgmq_available then
        create extension if not exists pgmq;
        raise notice 'Installed: pgmq';
    else
        raise notice 'pgmq not available - message queue features will not function';
        raise notice '(Install with: apt install postgresql-NN-pgmq)';
    end if;
end $$;

-- TimescaleDB: Time-series database extension (OPTIONAL)
-- Provides hypertables, compression, continuous aggregates, and retention policies
-- If not available, sessions will use regular tables instead.
do $$
declare
    tsdb_available boolean;
begin
    -- Check if TimescaleDB is available in the system
    select exists (
        select 1 from pg_available_extensions where name = 'timescaledb'
    ) into tsdb_available;

    if tsdb_available then
        create extension if not exists timescaledb;
        raise notice 'Installed: timescaledb';
    else
        raise notice 'TimescaleDB not available - sessions will use regular tables';
        raise notice '(This is fine for development/testing, but production should use TimescaleDB)';
    end if;
end $$;

\echo ''
\echo '=========================================='
\echo 'Extensions setup complete!'
\echo '=========================================='
\echo ''

-- Show what was installed
\echo 'Installed extensions:'
select extname, extversion from pg_extension
where extname in ('btree_gist', 'unaccent', 'pgtap', 'pgmq', 'timescaledb')
order by extname;

\echo ''
\echo 'Next step: Create ORES user'
\echo '  psql -U postgres -v ores_password=''SECRET'' -f setup_user.sql'
\echo ''
