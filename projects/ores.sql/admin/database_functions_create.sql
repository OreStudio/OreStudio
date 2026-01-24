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
 * This file is part of ores_admin database utilities.
 *
 * NOTE: PostgreSQL does not allow CREATE DATABASE inside a function/transaction.
 * These functions generate the SQL commands that need to be executed separately.
 */

-- Generates the SQL command to create a new database from template.
-- The returned command must be executed outside of any transaction.
-- Parameters:
--   db_name: Optional database name. If NULL, generates a whimsical name.
-- Returns: SQL command string to execute
create or replace function generate_create_database_sql(db_name text default null)
returns text as $$
declare
    final_name text;
begin
    if db_name is null then
        final_name := generate_unique_database_name_from_server();
    else
        final_name := db_name;
    end if;

    return format(
        e'-- create database from template\n'
        'create database %i with template = ores_template;\n'
        'grant all privileges on database %i to ores;\n'
        '\n'
        '-- Connect to new database and initialize\n'
        '\\c %i\n'
        '\\ir ./instance/init_instance.sql\n',
        final_name, final_name, final_name
    );
end;
$$ language plpgsql volatile;

-- Convenience function that outputs the command directly.
-- Usage: select create_database_command();
--        select create_database_command('my_custom_name');
create or replace function create_database_command(db_name text default null)
returns void as $$
begin
    raise notice e'\n%', generate_create_database_sql(db_name);
    raise notice e'\ncopy and paste the above commands to create the database.\n';
end;
$$ language plpgsql volatile;
