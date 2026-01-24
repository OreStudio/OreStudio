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
 *   -- Full wipe (no instance databases):
 *   psql -U postgres -f teardown_all.sql
 *
 *   -- With instance databases (explicit list required):
 *   psql -U postgres -v db_list="ores_happy_penguin,ores_dancing_fox" -f teardown_all.sql
 *
 * WHAT IT DROPS (in order):
 *   1. Test databases (pattern-based - acceptable for ephemeral DBs)
 *   2. Instance databases (from explicit db_list, if provided)
 *   3. ores_template (explicit)
 *   4. ores_admin (explicit)
 *   5. ores role (explicit)
 *
 * TO DISCOVER INSTANCE DATABASES:
 *   Before running teardown, list existing instances:
 *
 *   \c ores_admin
 *   SELECT * FROM admin_ores_instance_databases_view;
 *
 *   Then provide the list to this script. This ensures production drops
 *   are intentional and can be reviewed/checked-in before execution.
 */

\set ON_ERROR_STOP on

\echo ''
\echo '=============================================='
\echo 'ORES Complete Teardown'
\echo '=============================================='
\echo ''
\echo 'WARNING: This will remove ALL ORES components!'
\echo ''

--------------------------------------------------------------------------------
-- Step 1: Drop test databases (pattern-based is OK for ephemeral test DBs)
--------------------------------------------------------------------------------
\echo '--- Step 1: Dropping test databases ---'
\echo ''

SELECT COUNT(*) AS test_db_count
FROM pg_database
WHERE datname LIKE 'ores_test_%'
   OR datname LIKE 'oresdb_test_%'
\gset

\if :test_db_count
    \echo 'Found' :test_db_count 'test database(s) to drop.'

    SELECT format('DROP DATABASE IF EXISTS %I;', datname)
    FROM pg_database
    WHERE datname LIKE 'ores_test_%'
       OR datname LIKE 'oresdb_test_%'
    ORDER BY datname
    \gexec

    \echo 'Test databases dropped.'
\else
    \echo 'No test databases found.'
\endif

--------------------------------------------------------------------------------
-- Step 2: Drop instance databases (explicit list required)
--------------------------------------------------------------------------------
\echo ''
\echo '--- Step 2: Dropping instance databases ---'
\echo ''

\if :{?db_list}
    \echo 'Dropping instances from provided list: ' :db_list
    \echo ''

    -- Parse comma-separated list and drop each database
    -- Note: This uses a DO block to iterate over the list
    DO $$
    DECLARE
        db_name text;
        db_array text[];
    BEGIN
        -- Split the comma-separated list into an array
        db_array := string_to_array(:'db_list', ',');

        FOREACH db_name IN ARRAY db_array
        LOOP
            -- Trim whitespace
            db_name := trim(db_name);

            -- Skip empty entries
            IF db_name = '' THEN
                CONTINUE;
            END IF;

            -- Safety check: refuse to drop infrastructure DBs
            IF db_name IN ('ores_admin', 'ores_template', 'postgres', 'template0', 'template1') THEN
                RAISE NOTICE 'Skipping infrastructure database: %', db_name;
                CONTINUE;
            END IF;

            -- Terminate connections
            PERFORM pg_terminate_backend(pid)
            FROM pg_stat_activity
            WHERE datname = db_name AND pid <> pg_backend_pid();

            -- Drop the database
            EXECUTE format('DROP DATABASE IF EXISTS %I', db_name);
            RAISE NOTICE 'Dropped database: %', db_name;
        END LOOP;
    END;
    $$;

    \echo ''
    \echo 'Instance databases dropped.'
\else
    \echo 'No db_list provided. Skipping instance database drops.'
    \echo ''
    \echo 'To drop instance databases, first discover them:'
    \echo '  \c ores_admin'
    \echo '  SELECT * FROM admin_ores_instance_databases_view;'
    \echo ''
    \echo 'Then provide the list:'
    \echo '  psql -U postgres -v db_list="db1,db2,db3" -f teardown_all.sql'
\endif

--------------------------------------------------------------------------------
-- Step 3: Drop template
--------------------------------------------------------------------------------
\echo ''
\echo '--- Step 3: Dropping ores_template ---'

-- Unmark as template so it can be dropped
UPDATE pg_database SET datistemplate = false WHERE datname = 'ores_template';

-- Terminate connections
SELECT pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE datname = 'ores_template' AND pid <> pg_backend_pid();

DROP DATABASE IF EXISTS ores_template;
\echo 'ores_template dropped.'

--------------------------------------------------------------------------------
-- Step 4: Drop admin
--------------------------------------------------------------------------------
\echo ''
\echo '--- Step 4: Dropping ores_admin ---'

-- Terminate connections
SELECT pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE datname = 'ores_admin' AND pid <> pg_backend_pid();

DROP DATABASE IF EXISTS ores_admin;
\echo 'ores_admin dropped.'

--------------------------------------------------------------------------------
-- Step 5: Drop role
--------------------------------------------------------------------------------
\echo ''
\echo '--- Step 5: Dropping ores role ---'

DROP ROLE IF EXISTS ores;
\echo 'ores role dropped.'

\echo ''
\echo '=============================================='
\echo 'Teardown complete!'
\echo '=============================================='
\echo ''
\echo 'All ORES components have been removed.'
\echo ''
