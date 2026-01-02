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
 * Database Cleanup Functions
 *
 * Helper functions for cleaning up ORES databases.
 * This file is part of ores_admin database utilities.
 *
 * IMPORTANT: DROP DATABASE cannot run inside a transaction block, so the
 * cleanup functions that attempt to drop directly will fail. Instead, use
 * psql's \gexec feature to execute the generated DROP statements:
 *
 * To clean up TEST databases (ores_test_*, oresdb_test_*):
 *
 *   \c ores_admin
 *   SELECT format('DROP DATABASE IF EXISTS %I;', database_name)
 *   FROM test_databases \gexec
 *
 * To clean up ALL ORES databases (ores_*, oresdb_*):
 *
 *   \c ores_admin
 *   SELECT format('DROP DATABASE IF EXISTS %I;', database_name)
 *   FROM ores_databases \gexec
 *
 * To preview what will be deleted:
 *
 *   SELECT * FROM test_databases;
 *   SELECT * FROM ores_databases;
 */

--------------------------------------------------------------------------------
-- Views for identifying databases
--------------------------------------------------------------------------------

-- View that identifies all test databases on the server.
-- Test databases are created with patterns like 'ores_test_<pid>_<random>'
-- or 'oresdb_test_<pid>_<random>' and may be left behind if tests crash.
CREATE OR REPLACE VIEW test_databases AS
SELECT d.datname::TEXT AS database_name
FROM pg_database d
WHERE d.datname LIKE 'ores_test_%'
   OR d.datname LIKE 'oresdb_test_%'
ORDER BY d.datname;

-- View that identifies all ORES databases on the server.
-- This includes instance databases (ores_*, oresdb_*) and test databases.
-- Excludes infrastructure databases (ores, ores_admin, ores_template).
CREATE OR REPLACE VIEW ores_databases AS
SELECT d.datname::TEXT AS database_name
FROM pg_database d
WHERE (d.datname LIKE 'ores_%' OR d.datname LIKE 'oresdb_%')
  AND d.datname NOT IN ('ores', 'ores_admin', 'ores_template')
ORDER BY d.datname;

-- View that identifies ORES instance databases (excludes test, template, admin).
CREATE OR REPLACE VIEW ores_instance_databases AS
SELECT d.datname::TEXT AS database_name
FROM pg_database d
WHERE d.datname LIKE 'ores_%'
  AND d.datname NOT LIKE 'ores_test_%'
  AND d.datname NOT LIKE '%_template'
  AND d.datname NOT IN ('ores', 'ores_admin')
ORDER BY d.datname;

--------------------------------------------------------------------------------
-- List functions
--------------------------------------------------------------------------------

-- Lists all test databases on the server.
CREATE OR REPLACE FUNCTION list_test_databases()
RETURNS TABLE(database_name TEXT) AS $$
BEGIN
    RETURN QUERY SELECT td.database_name FROM test_databases td;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Lists all ORES databases on the server.
CREATE OR REPLACE FUNCTION list_ores_databases()
RETURNS TABLE(database_name TEXT) AS $$
BEGIN
    RETURN QUERY SELECT od.database_name FROM ores_databases od;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Lists all ORES instance databases (excludes test and template).
CREATE OR REPLACE FUNCTION list_ores_instance_databases()
RETURNS TABLE(database_name TEXT) AS $$
BEGIN
    RETURN QUERY SELECT oid.database_name FROM ores_instance_databases oid;
END;
$$ LANGUAGE plpgsql VOLATILE;

--------------------------------------------------------------------------------
-- SQL generation functions (for use with \gexec or manual execution)
--------------------------------------------------------------------------------

-- Generates SQL commands to drop all test databases.
CREATE OR REPLACE FUNCTION generate_cleanup_test_databases_sql()
RETURNS TEXT AS $$
DECLARE
    sql_commands TEXT;
    db_count INT;
BEGIN
    SELECT count(*),
           string_agg(format('DROP DATABASE IF EXISTS %I;', database_name),
                      E'\n' ORDER BY database_name)
    INTO db_count, sql_commands
    FROM test_databases;

    IF db_count = 0 THEN
        RETURN '-- No test databases found to clean up.';
    END IF;

    RETURN format(E'-- Found %s test database(s) to clean up:\n', db_count) ||
           sql_commands || E'\n';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Generates SQL commands to drop all ORES databases.
CREATE OR REPLACE FUNCTION generate_cleanup_ores_databases_sql()
RETURNS TEXT AS $$
DECLARE
    sql_commands TEXT;
    db_count INT;
BEGIN
    SELECT count(*),
           string_agg(format('DROP DATABASE IF EXISTS %I;', database_name),
                      E'\n' ORDER BY database_name)
    INTO db_count, sql_commands
    FROM ores_databases;

    IF db_count = 0 THEN
        RETURN '-- No ORES databases found to clean up.';
    END IF;

    RETURN format(E'-- Found %s ORES database(s) to clean up:\n', db_count) ||
           sql_commands || E'\n';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Generates SQL commands to drop all ORES instance databases.
CREATE OR REPLACE FUNCTION generate_cleanup_ores_instance_databases_sql()
RETURNS TEXT AS $$
DECLARE
    sql_commands TEXT;
    db_count INT;
BEGIN
    SELECT count(*),
           string_agg(format('DROP DATABASE IF EXISTS %I;', database_name),
                      E'\n' ORDER BY database_name)
    INTO db_count, sql_commands
    FROM ores_instance_databases;

    IF db_count = 0 THEN
        RETURN '-- No ORES instance databases found to clean up.';
    END IF;

    RETURN format(E'-- Found %s ORES instance database(s) to clean up:\n', db_count) ||
           sql_commands || E'\n';
END;
$$ LANGUAGE plpgsql VOLATILE;
