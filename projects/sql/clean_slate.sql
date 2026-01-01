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
 * Complete ORES Cleanup Script
 *
 * Removes ALL ORES-related databases to get a completely clean slate.
 * This includes: ores_admin, ores_template, and all instance/test databases.
 *
 * USAGE:
 *   psql -U postgres -f clean_slate.sql
 *
 * WARNING: This is DESTRUCTIVE and cannot be undone!
 *          Drops all databases AND the ores role.
 */

\set ON_ERROR_STOP on

\echo ''
\echo '=============================================='
\echo 'ORES Complete Cleanup'
\echo '=============================================='
\echo ''
\echo 'This will DROP all ORES databases!'
\echo ''

-- First, unmark ores_template as a template so it can be dropped
UPDATE pg_database SET datistemplate = false WHERE datname = 'ores_template';

-- Generate and execute DROP statements for all ORES databases
\echo 'Dropping all ORES databases...'
\echo ''

SELECT format('DROP DATABASE IF EXISTS %I;', datname)
FROM pg_database
WHERE datname LIKE 'ores_%'
   OR datname LIKE 'oresdb_%'
ORDER BY
    -- Drop instances first, then template, then admin
    CASE
        WHEN datname = 'ores_admin' THEN 3
        WHEN datname = 'ores_template' THEN 2
        ELSE 1
    END,
    datname
\gexec

-- Drop the ores role
\echo 'Dropping ores role...'
DROP ROLE IF EXISTS ores;

\echo ''
\echo '=============================================='
\echo 'Cleanup complete!'
\echo '=============================================='
\echo ''
\echo 'All ORES databases and the ores role have been removed.'
\echo ''
