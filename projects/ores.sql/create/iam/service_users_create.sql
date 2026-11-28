/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_service_users_create.mustache
 *
 * Creates/updates domain service database users (idempotent).
 * Included by recreate_database.sql (postgres superuser phase).
 *
 * A service role gets no password here: it resolves its own from the vault at
 * start, and compass vault provision writes it.
 *
 * Each one has a fixture login beside it, named <service user>_test, which
 * holds the deployment's test password and starts as the service's own role.
 * A test that acts as a service signs in as the fixture, so the grants it
 * exercises are the service's, without holding a password the environment file
 * does not carry.
 *
 * Required psql variables:
 *   :{name}_service_user, :service_role, :test_dml_password
 */

select set_config('ores.test_password', :'test_dml_password', false);

-- IAM domain service
select set_config('ores.cur_user', :'iam_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :iam_service_user;
alter  role :iam_service_user set search_path to public;

-- Reference Data domain service
select set_config('ores.cur_user', :'refdata_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :refdata_service_user;
alter  role :refdata_service_user set search_path to public;

-- Data Quality domain service
select set_config('ores.cur_user', :'dq_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :dq_service_user;
alter  role :dq_service_user set search_path to public;

-- Variability domain service
select set_config('ores.cur_user', :'variability_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :variability_service_user;
alter  role :variability_service_user set search_path to public;

-- Assets domain service
select set_config('ores.cur_user', :'assets_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :assets_service_user;
alter  role :assets_service_user set search_path to public;

-- Scheduler domain service
select set_config('ores.cur_user', :'scheduler_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :scheduler_service_user;
alter  role :scheduler_service_user set search_path to public;

-- Reporting domain service
select set_config('ores.cur_user', :'reporting_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :reporting_service_user;
alter  role :reporting_service_user set search_path to public;

-- Telemetry domain service
select set_config('ores.cur_user', :'telemetry_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :telemetry_service_user;
alter  role :telemetry_service_user set search_path to public;

-- Trading domain service
select set_config('ores.cur_user', :'trading_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :trading_service_user;
alter  role :trading_service_user set search_path to public;

-- Compute Grid domain service
select set_config('ores.cur_user', :'compute_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :compute_service_user;
alter  role :compute_service_user set search_path to public;

-- Synthetic domain service
select set_config('ores.cur_user', :'synthetic_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :synthetic_service_user;
alter  role :synthetic_service_user set search_path to public;

-- Workflow Orchestration domain service
select set_config('ores.cur_user', :'workflow_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :workflow_service_user;
alter  role :workflow_service_user set search_path to public;

-- ORE Import domain service
select set_config('ores.cur_user', :'ore_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :ore_service_user;
alter  role :ore_service_user set search_path to public;

-- Market Data domain service
select set_config('ores.cur_user', :'marketdata_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :marketdata_service_user;
alter  role :marketdata_service_user set search_path to public;

-- Analytics domain service
select set_config('ores.cur_user', :'analytics_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :analytics_service_user;
alter  role :analytics_service_user set search_path to public;

-- Object Storage domain service
select set_config('ores.cur_user', :'storage_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :storage_service_user;
alter  role :storage_service_user set search_path to public;

-- Inbox domain service
select set_config('ores.cur_user', :'inbox_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :inbox_service_user;
alter  role :inbox_service_user set search_path to public;

-- Vault domain service
select set_config('ores.cur_user', :'vault_service_user', false);
do $$
declare
    v_service text := current_setting('ores.cur_user');
    v_fixture text := current_setting('ores.cur_user') || '_test';
begin
    if not exists (select 1 from pg_roles where rolname = v_service) then
        execute format('create user %I', v_service); end if;
    if not exists (select 1 from pg_roles where rolname = v_fixture) then
        execute format('create user %I login', v_fixture); end if;
    execute format('alter role %I with password %L',
                   v_fixture, current_setting('ores.test_password'));
    execute format('grant %I to %I', v_service, v_fixture);
    execute format('alter role %I set role = %I', v_fixture, v_service);
end $$;
grant :service_role to :vault_service_user;
alter  role :vault_service_user set search_path to public;

