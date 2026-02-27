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
 * PostgreSQL Extensions Setup — postgres database only
 *
 * Installs extensions that must live in the postgres database and cannot be
 * installed in application databases. Must be run while connected to postgres.
 *
 * Called by create_database.sql before switching to the application database.
 *
 * EXTENSIONS:
 *   - pg_cron: Job scheduler. pg_cron's background worker reads job
 *     descriptions from the database configured as cron.database_name
 *     (defaults to 'postgres'). It can only be installed there.
 *     ores.scheduler uses cron.schedule_in_database() to schedule jobs that
 *     execute in each application database, so all environments on the same
 *     PostgreSQL cluster share one pg_cron installation here in postgres.
 *
 * PREREQUISITES FOR PG_CRON (pg_cron 1.4+):
 *   1. Install the package:
 *        Debian/Ubuntu: apt install postgresql-NN-cron
 *   2. Edit postgresql.conf:
 *        shared_preload_libraries = '...,pg_cron'
 *        (Leave cron.database_name unset — defaults to 'postgres', which is correct)
 *   3. Restart PostgreSQL:
 *        sudo systemctl restart postgresql
 *
 * USAGE:
 *   psql -U postgres -f setup_extensions_postgres.sql
 */

\set ON_ERROR_STOP on

\echo ''
\echo 'Installing postgres-database extensions...'
\echo ''

-- pg_cron: Job scheduler extension (OPTIONAL)
--
-- pg_cron requires two prerequisites beyond having the package installed:
--   1. pg_cron must be in shared_preload_libraries (so its GUCs are registered)
--   2. The extension must be created in cron.database_name (defaults to 'postgres')
--
-- We check both conditions before calling CREATE EXTENSION to avoid a hard
-- error from pg_cron's own install script when either prerequisite is missing.
do $$
declare
    pgcron_available boolean;
    pgcron_loaded boolean;
begin
    select exists (
        select 1 from pg_available_extensions where name = 'pg_cron'
    ) into pgcron_available;

    -- current_setting(..., missing_ok := true) returns NULL when the GUC does
    -- not exist, i.e. when pg_cron.so is not loaded via shared_preload_libraries.
    pgcron_loaded := (current_setting('cron.database_name', true) is not null);

    if pgcron_available and pgcron_loaded then
        create extension if not exists pg_cron;
        raise notice 'Installed: pg_cron';
    elsif pgcron_available and not pgcron_loaded then
        raise notice 'pg_cron is installed but not loaded in shared_preload_libraries';
        raise notice 'ores.scheduler job scheduling will not function';
        raise notice '(Add pg_cron to shared_preload_libraries and restart PostgreSQL)';
    else
        raise notice 'pg_cron not available - ores.scheduler job scheduling will not function';
        raise notice '(Install with: apt install postgresql-NN-cron, then configure postgresql.conf)';
    end if;
end $$;

\echo ''
\echo '=========================================='
\echo 'Postgres-database extensions setup complete!'
\echo '=========================================='
\echo ''
