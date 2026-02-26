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
 * Recreate Database from Scratch (Postgres Superuser Phase)
 *
 * Drops and recreates only the target database. Shared roles and users are
 * created idempotently so that sibling databases (e.g. other local dev
 * environments) are not disturbed.
 *
 * USAGE:
 *   Typically called via recreate_database.sh (handles confirmation and DDL phase):
 *     ./recreate_database.sh -p postgres_pass -d ddl_pass -c cli_pass \
 *       -w wt_pass -m comms_pass -h http_pass -t test_ddl_pass \
 *       -T test_dml_pass -r ro_pass [-y] [--no-sql-validation]
 *
 *   Direct psql usage:
 *     psql -U postgres \
 *       -v ddl_password='...' -v cli_password='...' -v wt_password='...' \
 *       -v comms_password='...' -v http_password='...' \
 *       -v test_ddl_password='...' -v test_dml_password='...' \
 *       -v ro_password='...' -v db_name='...' \
 *       -f recreate_database.sql
 *
 * Variables:
 *   :ddl_password      - password for DDL operations
 *   :cli_password      - password for CLI service
 *   :wt_password       - password for Web Toolkit service
 *   :comms_password    - password for Communications service
 *   :http_password     - password for HTTP service
 *   :test_ddl_password - password for test DDL operations
 *   :test_dml_password - password for test DML operations
 *   :ro_password       - password for read-only access
 *   :db_name           - database name to create
 */
\pset pager off
\pset tuples_only on
\timing off

\set ON_ERROR_STOP on

--------------------------------------------------------------------------------
-- Step 1: Drop only the target database
--------------------------------------------------------------------------------
\echo ''
\echo '--- Dropping target database:' :db_name '---'
\echo ''

drop database if exists :db_name;

\echo 'Target database dropped (if it existed).'
\echo ''

--------------------------------------------------------------------------------
-- Step 2: Create/update group roles (idempotent via DO block)
--------------------------------------------------------------------------------
\echo '--- Creating/updating ORES roles ---'
\echo ''

do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_owner') then
        create role ores_owner nologin;
    end if;
    if not exists (select 1 from pg_roles where rolname = 'ores_rw') then
        create role ores_rw nologin;
    end if;
    if not exists (select 1 from pg_roles where rolname = 'ores_ro') then
        create role ores_ro nologin;
    end if;
end
$$;

\echo 'Group roles ready.'
\echo ''

--------------------------------------------------------------------------------
-- Step 3: Create/update service users (idempotent)
--
-- DO blocks handle conditional creation. ALTER USER (outside the block) is used
-- to always update passwords, since psql variable substitution (:'var') works
-- in top-level statements but not inside DO blocks.
-- GRANT role TO user is idempotent (issues a WARNING if already granted, not an error).
-- ALTER ROLE ... SET is always idempotent.
--------------------------------------------------------------------------------
\echo '--- Creating/updating ORES service users ---'
\echo ''

-- DDL user (schema migrations)
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_ddl_user') then
        create user ores_ddl_user;
    end if;
end
$$;
alter  user ores_ddl_user with password :'ddl_password';
grant  ores_owner to ores_ddl_user;
alter  role ores_ddl_user set search_path to public;

-- CLI user
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_cli_user') then
        create user ores_cli_user;
    end if;
end
$$;
alter  user ores_cli_user with password :'cli_password';
grant  ores_rw to ores_cli_user;
alter  role ores_cli_user set search_path to public;

-- Web Toolkit user
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_wt_user') then
        create user ores_wt_user;
    end if;
end
$$;
alter  user ores_wt_user with password :'wt_password';
grant  ores_rw to ores_wt_user;
alter  role ores_wt_user set search_path to public;

-- Communications user
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_comms_user') then
        create user ores_comms_user;
    end if;
end
$$;
alter  user ores_comms_user with password :'comms_password';
grant  ores_rw to ores_comms_user;
alter  role ores_comms_user set search_path to public;

-- HTTP user
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_http_user') then
        create user ores_http_user;
    end if;
end
$$;
alter  user ores_http_user with password :'http_password';
grant  ores_rw to ores_http_user;
alter  role ores_http_user set search_path to public;

-- Test DDL user (can create databases for isolation)
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_test_ddl_user') then
        create user ores_test_ddl_user createdb;
    end if;
end
$$;
alter  user ores_test_ddl_user with password :'test_ddl_password';
grant  ores_owner to ores_test_ddl_user;
alter  role ores_test_ddl_user set search_path to public;
alter  role ores_test_ddl_user set app.current_tenant_id = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

-- Test DML user
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_test_dml_user') then
        create user ores_test_dml_user;
    end if;
end
$$;
alter  user ores_test_dml_user with password :'test_dml_password';
grant  ores_rw to ores_test_dml_user;
alter  role ores_test_dml_user set search_path to public;
alter  role ores_test_dml_user set app.current_tenant_id = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

-- Read-only user (for devs/BI)
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_readonly_user') then
        create user ores_readonly_user;
    end if;
end
$$;
alter  user ores_readonly_user with password :'ro_password';
grant  ores_ro to ores_readonly_user;
alter  role ores_readonly_user set search_path to public;

\echo 'Service users ready.'
\echo ''


