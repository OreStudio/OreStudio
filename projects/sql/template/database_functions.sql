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
 * Database Management Functions
 *
 * Helper functions for creating and managing ORES database instances.
 *
 * NOTE: PostgreSQL does not allow CREATE DATABASE inside a function/transaction.
 * These functions generate the SQL commands that need to be executed separately.
 */

SET search_path TO ores;

-- Generates the SQL command to create a new database from template.
-- The returned command must be executed outside of any transaction.
-- Parameters:
--   db_name: Optional database name. If NULL, generates a whimsical name.
-- Returns: SQL command string to execute
CREATE OR REPLACE FUNCTION ores.generate_create_database_sql(db_name TEXT DEFAULT NULL)
RETURNS TEXT AS $$
DECLARE
    final_name TEXT;
BEGIN
    IF db_name IS NULL THEN
        final_name := ores.generate_unique_database_name_from_server();
    ELSE
        final_name := db_name;
    END IF;

    RETURN format(
        E'-- Create database from template\n'
        'CREATE DATABASE %I WITH TEMPLATE = ores_template;\n'
        'GRANT ALL PRIVILEGES ON DATABASE %I TO ores;\n'
        '\n'
        '-- Connect to new database and initialize\n'
        '\\c %I\n'
        '\\ir ./instance/init_instance.sql\n',
        final_name, final_name, final_name
    );
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Convenience function that outputs the command directly.
-- Usage: SELECT ores.create_database_command();
--        SELECT ores.create_database_command('my_custom_name');
CREATE OR REPLACE FUNCTION ores.create_database_command(db_name TEXT DEFAULT NULL)
RETURNS VOID AS $$
BEGIN
    RAISE NOTICE E'\n%', ores.generate_create_database_sql(db_name);
    RAISE NOTICE E'\nCopy and paste the above commands to create the database.\n';
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Lists all ORES databases on the server.
-- NOTE: last_modified is the modification time of the database's directory,
-- which approximates creation time but can be updated by maintenance operations.
CREATE OR REPLACE FUNCTION ores.list_ores_databases()
RETURNS TABLE(database_name TEXT, last_modified TIMESTAMP WITH TIME ZONE) AS $$
BEGIN
    RETURN QUERY
    SELECT
        d.datname::TEXT,
        (pg_stat_file('base/' || d.oid)).modification AS last_modified
    FROM pg_database d
    WHERE d.datname LIKE 'ores_%'
      AND d.datname != 'ores_template'
    ORDER BY last_modified DESC;
END;
$$ LANGUAGE plpgsql VOLATILE;
