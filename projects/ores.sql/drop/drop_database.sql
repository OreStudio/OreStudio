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
 * Drop a Single Database
 *
 * Drops a specific database by name. First runs the schema drop scripts
 * to validate drop logic, then drops the database itself.
 *
 * USAGE:
 *   psql -U postgres -v db_name='ores_happy_penguin' -f drop/drop_database.sql
 *
 * SAFETY:
 *   - Requires explicit database name (no pattern matching)
 *   - Validates database exists before dropping
 *   - Refuses to drop infrastructure DBs (use dedicated teardown scripts)
 *   - Runs schema drops first to validate drop logic
 */

\set ON_ERROR_STOP on

-- Validate db_name is provided
\if :{?db_name}
\else
    \echo ''
    \echo 'ERROR: db_name variable is required'
    \echo ''
    \echo 'Usage: psql -U postgres -v db_name=''your_database'' -f drop/drop_database.sql'
    \echo ''
    \quit
\endif

-- Check if database exists
select exists(select 1 from pg_database where datname = :'db_name') as db_exists \gset

\if :db_exists
\else
    \echo ''
    \echo 'ERROR: Database does not exist:' :db_name
    \echo ''
    \quit
\endif

-- Refuse to drop infrastructure databases (use dedicated teardown scripts)
select case
    when :'db_name' in ('ores_admin', 'ores_template')
    then true
    else false
end as is_infrastructure \gset

\if :is_infrastructure
    \echo ''
    \echo 'ERROR: Cannot drop infrastructure database:' :db_name
    \echo ''
    \echo 'Use the dedicated teardown scripts instead:'
    \echo '  - admin/teardown_admin.sql for ores_admin'
    \echo '  - teardown_template.sql for ores_template'
    \echo ''
    \quit
\endif

-- Refuse to drop system databases
select case
    when :'db_name' in ('postgres', 'template0', 'template1')
    then true
    else false
end as is_system \gset

\if :is_system
    \echo ''
    \echo 'ERROR: Cannot drop system database:' :db_name
    \echo ''
    \quit
\endif

\echo ''
\echo '=============================================='
\echo 'Dropping database:' :db_name
\echo '=============================================='
\echo ''

-- Step 1: Connect and run schema drops to validate drop logic
\echo '--- Step 1: Dropping schema objects ---'
\c :db_name
\ir ./drop.sql

-- Step 2: Disconnect and drop the database
\echo ''
\echo '--- Step 2: Dropping database ---'
\c postgres

-- Unmark as template if needed (in case it was marked)
update pg_database set datistemplate = false where datname = :'db_name';

-- Terminate any remaining connections
select pg_terminate_backend(pid)
from pg_stat_activity
where datname = :'db_name' and pid <> pg_backend_pid();

-- Drop the database
drop database :db_name;

\echo ''
\echo 'Database dropped successfully:' :db_name
\echo ''
