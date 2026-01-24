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
 * Complete ORES Teardown
 *
 * Removes ALL ORES components from the PostgreSQL cluster.
 * This script is explicit about what it drops for full traceability.
 *
 * USAGE:
 *   psql -U postgres -f teardown_all.sql
 *
 *   -- Skip confirmation prompt (for automated/dev use):
 *   psql -U postgres -v skip_confirm=1 -f teardown_all.sql
 *
 * WHAT IT DROPS (in order):
 *   1. Test databases (pattern-based - acceptable for ephemeral DBs)
 *   2. Instance databases (from teardown_instances.sql - explicit, reviewable)
 *   3. ores_template (explicit)
 *   4. ores_admin (explicit)
 *   5. ores role (explicit)
 *
 * INSTANCE DATABASE WORKFLOW:
 *   For production environments, instance drops must be explicit and reviewable:
 *
 *   1. Generate the teardown script:
 *      psql -U postgres -f admin/admin_teardown_instances_generate.sql
 *
 *   2. Review the generated file:
 *      cat teardown_instances.sql
 *
 *   3. Commit to git for audit trail:
 *      git add teardown_instances.sql && git commit -m "Teardown: list DBs to drop"
 *
 *   4. Run this script:
 *      psql -U postgres -f teardown_all.sql
 *
 * If teardown_instances.sql is empty or contains only comments, no instance
 * databases will be dropped (useful for dev environments with no instances).
 */

\set ON_ERROR_STOP on

\echo ''
\echo '=============================================='
\echo 'ORES Complete Teardown'
\echo '=============================================='
\echo ''
\echo 'WARNING: This will remove ALL ORES components!'
\echo ''

-- Check if confirmation should be skipped (for automated/dev use)
\if :{?skip_confirm}
    \echo 'Confirmation skipped.'
    \echo ''
\else
    \echo 'Before proceeding, ensure you have:'
    \echo '  - Generated teardown_instances.sql (if dropping instances)'
    \echo '  - Reviewed all databases that will be dropped'
    \echo '  - Committed the teardown script for audit trail (production)'
    \echo ''

    -- Confirmation prompt
    \prompt 'Type "yes" to proceed with teardown: ' confirm_teardown

    -- Check confirmation (case-insensitive)
    select case
        when lower(:'confirm_teardown') = 'yes' then false
        else true
    end as abort_teardown \gset

    \if :abort_teardown
        \echo ''
        \echo 'Teardown aborted. You must type "yes" to proceed.'
        \echo ''
        \quit
    \endif
\endif

\echo 'Proceeding with teardown...'
\echo ''

--------------------------------------------------------------------------------
-- Step 1: Drop test databases (pattern-based is OK for ephemeral test DBs)
--------------------------------------------------------------------------------
\echo '--- Step 1: Dropping test databases ---'
\echo ''

select count(*) as test_db_count
from pg_database
where datname like 'ores_test_%'
   or datname like 'oresdb_test_%'
\gset

\if :test_db_count
    \echo 'Found' :test_db_count 'test database(s) to drop.'

    select format('drop database if exists %I;', datname)
    from pg_database
    where datname like 'ores_test_%'
       or datname like 'oresdb_test_%'
    order by datname
    \gexec

    \echo 'Test databases dropped.'
\else
    \echo 'No test databases found.'
\endif

--------------------------------------------------------------------------------
-- Step 2: Drop instance databases (from teardown_instances.sql)
--------------------------------------------------------------------------------
\echo ''
\echo '--- Step 2: Dropping instance databases ---'
\echo ''
\echo 'Running teardown_instances.sql...'
\echo ''

-- Include the generated instance teardown script
-- This file should contain explicit DROP DATABASE statements
-- If empty or not generated, no instances will be dropped
\ir teardown_instances.sql

\echo ''
\echo 'Instance database teardown complete.'

--------------------------------------------------------------------------------
-- Step 3: Drop template
--------------------------------------------------------------------------------
\echo ''
\echo '--- Step 3: Dropping ores_template ---'

-- Unmark as template so it can be dropped
update pg_database set datistemplate = false where datname = 'ores_template';

-- Terminate connections
select pg_terminate_backend(pid)
from pg_stat_activity
where datname = 'ores_template' and pid <> pg_backend_pid();

drop database if exists ores_template;
\echo 'ores_template dropped.'

--------------------------------------------------------------------------------
-- Step 4: Drop admin
--------------------------------------------------------------------------------
\echo ''
\echo '--- Step 4: Dropping ores_admin ---'

-- Terminate connections
select pg_terminate_backend(pid)
from pg_stat_activity
where datname = 'ores_admin' and pid <> pg_backend_pid();

drop database if exists ores_admin;
\echo 'ores_admin dropped.'

--------------------------------------------------------------------------------
-- Step 5: Drop role
--------------------------------------------------------------------------------
\echo ''
\echo '--- Step 5: Dropping ores role ---'

drop role if exists ores;
\echo 'ores role dropped.'

\echo ''
\echo '=============================================='
\echo 'Teardown complete!'
\echo '=============================================='
\echo ''
\echo 'All ORES components have been removed.'
\echo ''
