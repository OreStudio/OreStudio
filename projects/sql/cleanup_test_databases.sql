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
 * Test Database Cleanup Functions
 *
 * Helper functions for cleaning up leftover test databases.
 * Test databases are created with patterns like 'ores_test_<pid>_<random>'
 * or 'oresdb_test_<pid>_<random>' and may be left behind if tests crash
 * or are interrupted.
 */

-- Lists all test databases on the server.
-- Returns database name and last modification time.
CREATE OR REPLACE FUNCTION list_test_databases()
RETURNS TABLE(database_name TEXT, last_modified TIMESTAMP WITH TIME ZONE) AS $$
BEGIN
    RETURN QUERY
    SELECT
        d.datname::TEXT,
        (pg_stat_file('base/' || d.oid)).modification AS last_modified
    FROM pg_database d
    WHERE d.datname LIKE 'ores_test_%'
       OR d.datname LIKE 'oresdb_test_%'
    ORDER BY last_modified DESC;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Generates SQL commands to drop all test databases.
-- The returned commands must be executed outside of any transaction.
-- Parameters:
--   dry_run: If TRUE, just returns the commands without executing (default: TRUE)
-- Returns: SQL commands to execute
CREATE OR REPLACE FUNCTION generate_cleanup_test_databases_sql()
RETURNS TEXT AS $$
DECLARE
    db_record RECORD;
    sql_commands TEXT := '';
    db_count INT := 0;
BEGIN
    FOR db_record IN
        SELECT datname::TEXT AS name
        FROM pg_database
        WHERE datname LIKE 'ores_test_%'
           OR datname LIKE 'oresdb_test_%'
        ORDER BY datname
    LOOP
        sql_commands := sql_commands ||
            format('DROP DATABASE IF EXISTS %I;', db_record.name) || E'\n';
        db_count := db_count + 1;
    END LOOP;

    IF db_count = 0 THEN
        RETURN '-- No test databases found to clean up.';
    END IF;

    RETURN format(E'-- Found %s test database(s) to clean up:\n', db_count) ||
           sql_commands;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Convenience function that outputs the cleanup commands.
-- Usage: SELECT cleanup_test_databases_command();
CREATE OR REPLACE FUNCTION cleanup_test_databases_command()
RETURNS VOID AS $$
BEGIN
    RAISE NOTICE E'\n%', generate_cleanup_test_databases_sql();
    RAISE NOTICE E'\nCopy and paste the above commands to clean up test databases.\n';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Drops all test databases directly.
-- WARNING: This will immediately drop all test databases without confirmation.
-- Use list_test_databases() first to review what will be deleted.
CREATE OR REPLACE FUNCTION cleanup_test_databases()
RETURNS TABLE(database_name TEXT, status TEXT) AS $$
DECLARE
    db_record RECORD;
    drop_sql TEXT;
BEGIN
    FOR db_record IN
        SELECT datname::TEXT AS name
        FROM pg_database
        WHERE datname LIKE 'ores_test_%'
           OR datname LIKE 'oresdb_test_%'
        ORDER BY datname
    LOOP
        BEGIN
            -- First terminate any active connections
            PERFORM pg_terminate_backend(pid)
            FROM pg_stat_activity
            WHERE datname = db_record.name
              AND pid <> pg_backend_pid();

            -- Drop the database
            drop_sql := format('DROP DATABASE IF EXISTS %I', db_record.name);
            EXECUTE drop_sql;

            database_name := db_record.name;
            status := 'dropped';
            RETURN NEXT;
        EXCEPTION WHEN OTHERS THEN
            database_name := db_record.name;
            status := 'error: ' || SQLERRM;
            RETURN NEXT;
        END;
    END LOOP;
END;
$$ LANGUAGE plpgsql VOLATILE;
