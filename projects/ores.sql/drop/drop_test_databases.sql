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
 * Drop All Test Databases
 *
 * Cleans up test databases that may have been left behind by crashed tests.
 * Uses pattern matching because test DBs follow strict naming conventions
 * and are ephemeral by design.
 *
 * USAGE:
 *   psql -U postgres -f drop/drop_test_databases.sql
 *
 * TEST DATABASE PATTERNS:
 *   - ores_test_<pid>_<random>
 *   - oresdb_test_<pid>_<random>
 *
 * NOTE: Pattern matching is acceptable here because:
 *   - Test DBs are ephemeral by design
 *   - They follow a strict naming convention
 *   - We explicitly WANT to sweep up any orphaned test DBs
 */

\set ON_ERROR_STOP on

\echo ''
\echo '=============================================='
\echo 'Test Database Cleanup'
\echo '=============================================='
\echo ''

-- Preview what will be dropped
\echo 'Test databases found:'
\echo ''

select datname as database_name
from pg_database
where datname like 'ores_test_%'
   or datname like 'oresdb_test_%'
order by datname;

-- Count databases
select count(*) as test_db_count
from pg_database
where datname like 'ores_test_%'
   or datname like 'oresdb_test_%'
\gset

\if :test_db_count
    \echo ''
    \echo 'Dropping' :test_db_count 'test database(s)...'
    \echo ''

    -- Generate and execute DROP statements
    select format('drop database if exists %I;', datname)
    from pg_database
    where datname like 'ores_test_%'
       or datname like 'oresdb_test_%'
    order by datname
    \gexec

    \echo ''
    \echo 'Test database cleanup complete.'
\else
    \echo ''
    \echo 'No test databases found.'
\endif

\echo ''
