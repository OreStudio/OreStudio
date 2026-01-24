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
 *   FROM admin_test_databases_view \gexec
 *
 * To clean up ALL ORES databases (ores_*, oresdb_*):
 *
 *   \c ores_admin
 *   SELECT format('DROP DATABASE IF EXISTS %I;', database_name)
 *   FROM admin_ores_databases_view \gexec
 *
 * To preview what will be deleted:
 *
 *   SELECT * FROM admin_test_databases_view;
 *   SELECT * FROM admin_ores_databases_view;
 */

--------------------------------------------------------------------------------
-- Views for identifying databases
--------------------------------------------------------------------------------

-- View that identifies all test databases on the server.
-- Test databases are created with patterns like 'ores_test_<pid>_<random>'
-- or 'oresdb_test_<pid>_<random>' and may be left behind if tests crash.
create or replace view admin_test_databases_view as
select d.datname::text as database_name
from pg_database d
where d.datname like 'ores_test_%'
   or d.datname like 'oresdb_test_%'
order by d.datname;

-- View that identifies all ORES databases on the server.
-- This includes instance databases (ores_*, oresdb_*) and test databases.
-- Excludes infrastructure databases (ores, ores_admin, ores_template).
create or replace view admin_ores_databases_view as
select d.datname::text as database_name
from pg_database d
where (d.datname like 'ores_%' or d.datname like 'oresdb_%')
  and d.datname not in ('ores', 'ores_admin', 'ores_template')
order by d.datname;

-- View that identifies ORES instance databases (excludes test, template, admin).
create or replace view admin_ores_instance_databases_view as
select d.datname::text as database_name
from pg_database d
where d.datname like 'ores_%'
  and d.datname not like 'ores_test_%'
  and d.datname not like '%_template'
  and d.datname not in ('ores', 'ores_admin')
order by d.datname;

--------------------------------------------------------------------------------
-- List functions
--------------------------------------------------------------------------------

-- Lists all test databases on the server.
create or replace function admin_list_test_databases_fn()
returns table(database_name text) as $$
begin
    return query select td.database_name from admin_test_databases_view td;
end;
$$ language plpgsql volatile;

-- Lists all ORES databases on the server.
create or replace function admin_list_ores_databases_fn()
returns table(database_name text) as $$
begin
    return query select od.database_name from admin_ores_databases_view od;
end;
$$ language plpgsql volatile;

-- Lists all ORES instance databases (excludes test and template).
create or replace function admin_list_ores_instance_databases_fn()
returns table(database_name text) as $$
begin
    return query select oid.database_name from admin_ores_instance_databases_view oid;
end;
$$ language plpgsql volatile;

--------------------------------------------------------------------------------
-- SQL generation functions (for use with \gexec or manual execution)
--------------------------------------------------------------------------------

-- Generates SQL commands to drop all test databases.
create or replace function admin_generate_cleanup_test_databases_sql_fn()
returns text as $$
declare
    sql_commands text;
    db_count int;
begin
    select count(*),
           string_agg(format('drop database if exists %I;', database_name),
                      e'\n' order by database_name)
    into db_count, sql_commands
    from admin_test_databases_view;

    if db_count = 0 then
        return '-- No test databases found to clean up.';
    end if;

    return format(e'-- Found %s test database(s) to clean up:\n', db_count) ||
           sql_commands || e'\n';
end;
$$ language plpgsql volatile;

-- Generates SQL commands to drop all ORES databases.
create or replace function admin_generate_cleanup_ores_databases_sql_fn()
returns text as $$
declare
    sql_commands text;
    db_count int;
begin
    select count(*),
           string_agg(format('drop database if exists %I;', database_name),
                      e'\n' order by database_name)
    into db_count, sql_commands
    from admin_ores_databases_view;

    if db_count = 0 then
        return '-- No ORES databases found to clean up.';
    end if;

    return format(e'-- Found %s ORES database(s) to clean up:\n', db_count) ||
           sql_commands || e'\n';
end;
$$ language plpgsql volatile;

-- Generates SQL commands to drop all ORES instance databases.
create or replace function admin_generate_cleanup_ores_instance_databases_sql_fn()
returns text as $$
declare
    sql_commands text;
    db_count int;
begin
    select count(*),
           string_agg(format('drop database if exists %I;', database_name),
                      e'\n' order by database_name)
    into db_count, sql_commands
    from admin_ores_instance_databases_view;

    if db_count = 0 then
        return '-- No ORES instance databases found to clean up.';
    end if;

    return format(e'-- Found %s ORES instance database(s) to clean up:\n', db_count) ||
           sql_commands || e'\n';
end;
$$ language plpgsql volatile;
