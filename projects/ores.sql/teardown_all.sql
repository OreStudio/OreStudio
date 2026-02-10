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
 *   2. Instance and dev databases (pattern-based discovery)
 *   3. ores users and roles (explicit)
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
    \echo '  - Reviewed all databases that will be dropped'
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
-- Step 2: Drop instance and dev databases (pattern-based discovery)
--------------------------------------------------------------------------------
\echo ''
\echo '--- Step 2: Dropping ORES databases ---'
\echo ''

select count(*) as ores_db_count
from pg_database
where (datname like 'ores_%' or datname like 'oresdb_%')
  and datname not like 'ores_test_%'
  and datname not like 'oresdb_test_%'
\gset

\if :ores_db_count
    \echo 'Found' :ores_db_count 'ORES database(s) to drop.'

    select format('drop database if exists %I;', datname)
    from pg_database
    where (datname like 'ores_%' or datname like 'oresdb_%')
      and datname not like 'ores_test_%'
      and datname not like 'oresdb_test_%'
    order by datname
    \gexec

    \echo 'ORES databases dropped.'
\else
    \echo 'No ORES databases found.'
\endif

--------------------------------------------------------------------------------
-- Step 3: Drop users and roles
--------------------------------------------------------------------------------
\echo ''
\echo '--- Step 3: Dropping ores users and roles ---'

-- Drop service users first (they depend on roles)
drop role if exists ores_ddl_user;
drop role if exists ores_cli_user;
drop role if exists ores_wt_user;
drop role if exists ores_comms_user;
drop role if exists ores_http_user;
drop role if exists ores_test_ddl_user;
drop role if exists ores_test_dml_user;
drop role if exists ores_readonly_user;
\echo 'Service users dropped.'

-- Drop group roles
drop role if exists ores_owner;
drop role if exists ores_rw;
drop role if exists ores_ro;
\echo 'Group roles dropped.'

-- Drop legacy role if it exists (for backwards compatibility)
drop role if exists ores;
\echo 'Legacy ores role dropped (if existed).'

\echo ''
\echo '=============================================='
\echo 'Teardown complete!'
\echo '=============================================='
\echo ''
\echo 'All ORES components have been removed.'
\echo ''
