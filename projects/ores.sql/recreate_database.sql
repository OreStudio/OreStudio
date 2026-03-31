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
 * Drops and recreates only the target database. Roles are created
 * idempotently and scoped to this environment via parameterised names,
 * so sibling databases (e.g. local1, local2) are never disturbed.
 *
 * USAGE: Typically called via recreate_database.sh.
 *
 * Variables (role names + passwords):
 *   :owner_role, :rw_role, :ro_role, :service_role
 *   :ddl_user, :cli_user, :wt_user, :shell_user, :http_user
 *   :test_ddl_user, :test_dml_user, :readonly_user
 *   :iam_service_user, :refdata_service_user, :dq_service_user,
 *   :variability_service_user, :assets_service_user,
 *   :synthetic_service_user, :scheduler_service_user,
 *   :reporting_service_user, :telemetry_service_user,
 *   :trading_service_user, :compute_service_user
 *   (one *_password variable per user)
 *   :db_name
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
-- Step 2: Create/update group roles (idempotent, env-scoped)
--------------------------------------------------------------------------------
\echo '--- Creating/updating ORES roles ---'
\echo ''

-- Pass role names via session config so they're readable inside DO blocks
-- (psql does not substitute :variables inside $$-quoted strings)
select set_config('ores.owner_role',   :'owner_role',   false);
select set_config('ores.rw_role',      :'rw_role',      false);
select set_config('ores.ro_role',      :'ro_role',      false);
select set_config('ores.service_role', :'service_role', false);

do $$
declare
    v_owner   text := current_setting('ores.owner_role');
    v_rw      text := current_setting('ores.rw_role');
    v_ro      text := current_setting('ores.ro_role');
    v_service text := current_setting('ores.service_role');
begin
    if not exists (select 1 from pg_roles where rolname = v_owner) then
        execute format('create role %I nologin', v_owner);
    end if;
    if not exists (select 1 from pg_roles where rolname = v_rw) then
        execute format('create role %I nologin', v_rw);
    end if;
    if not exists (select 1 from pg_roles where rolname = v_ro) then
        execute format('create role %I nologin', v_ro);
    end if;
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create role %I nologin', v_service);
    end if;
end $$;

\echo 'Group roles ready.'
\echo ''

--------------------------------------------------------------------------------
-- Step 3: Create/update service users (idempotent, env-scoped)
--
-- Each block: create if missing, then ALTER to sync password/roles/settings.
-- format('%I') handles quoting; :'var' injects passwords safely.
--------------------------------------------------------------------------------
\echo '--- Creating/updating ORES service users ---'
\echo ''

-- Helper macro: set ores.cur_user before each DO block so the block can
-- check existence and create the user without psql variable substitution.

-- DDL user
select set_config('ores.cur_user', :'ddl_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :ddl_user with password :'ddl_password';
grant :owner_role to :ddl_user;
alter  role :ddl_user set search_path to public;

-- CLI user
select set_config('ores.cur_user', :'cli_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :cli_user with password :'cli_password';
grant :rw_role to :cli_user;
alter  role :cli_user set search_path to public;

-- Web Toolkit user
select set_config('ores.cur_user', :'wt_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :wt_user with password :'wt_password';
grant :rw_role to :wt_user;
alter  role :wt_user set search_path to public;

-- Communications user
select set_config('ores.cur_user', :'shell_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :shell_user with password :'shell_password';
grant :rw_role to :shell_user;
alter  role :shell_user set search_path to public;

-- Domain service users — generated from service registry model.
-- To add a service: update projects/ores.codegen/models/services/ores_services_service_registry.json
-- and regenerate with: ./run_generator.sh models/services/... --profile service-registry
\i create/iam/service_users_create.sql

-- HTTP user
select set_config('ores.cur_user', :'http_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :http_user with password :'http_password';
grant :rw_role to :http_user;
alter  role :http_user set search_path to public;

-- Test DDL user (can create databases for isolation)
select set_config('ores.cur_user', :'test_ddl_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I createdb', current_setting('ores.cur_user')); end if; end $$;
alter  user :test_ddl_user with password :'test_ddl_password';
grant :owner_role to :test_ddl_user;
alter  role :test_ddl_user set search_path to public;
alter  role :test_ddl_user set app.current_tenant_id = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

-- Test DML user
select set_config('ores.cur_user', :'test_dml_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :test_dml_user with password :'test_dml_password';
grant :rw_role to :test_dml_user;
alter  role :test_dml_user set search_path to public;
alter  role :test_dml_user set app.current_tenant_id = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

-- Read-only user (for devs/BI)
select set_config('ores.cur_user', :'readonly_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :readonly_user with password :'ro_password';
grant :ro_role to :readonly_user;
alter  role :readonly_user set search_path to public;

\echo 'Service users ready.'
\echo ''
