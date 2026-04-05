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
 * Required psql variables (one pair per service):
 *   :{name}_service_user, :{name}_service_password
 *   :service_role
 */

-- IAM domain service
select set_config('ores.cur_user', :'iam_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :iam_service_user with password :'iam_service_password';
grant :service_role to :iam_service_user;
alter  role :iam_service_user set search_path to public;

-- Reference Data domain service
select set_config('ores.cur_user', :'refdata_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :refdata_service_user with password :'refdata_service_password';
grant :service_role to :refdata_service_user;
alter  role :refdata_service_user set search_path to public;

-- Data Quality domain service
select set_config('ores.cur_user', :'dq_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :dq_service_user with password :'dq_service_password';
grant :service_role to :dq_service_user;
alter  role :dq_service_user set search_path to public;

-- Variability domain service
select set_config('ores.cur_user', :'variability_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :variability_service_user with password :'variability_service_password';
grant :service_role to :variability_service_user;
alter  role :variability_service_user set search_path to public;

-- Assets domain service
select set_config('ores.cur_user', :'assets_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :assets_service_user with password :'assets_service_password';
grant :service_role to :assets_service_user;
alter  role :assets_service_user set search_path to public;

-- Scheduler domain service
select set_config('ores.cur_user', :'scheduler_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :scheduler_service_user with password :'scheduler_service_password';
grant :service_role to :scheduler_service_user;
alter  role :scheduler_service_user set search_path to public;

-- Reporting domain service
select set_config('ores.cur_user', :'reporting_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :reporting_service_user with password :'reporting_service_password';
grant :service_role to :reporting_service_user;
alter  role :reporting_service_user set search_path to public;

-- Telemetry domain service
select set_config('ores.cur_user', :'telemetry_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :telemetry_service_user with password :'telemetry_service_password';
grant :service_role to :telemetry_service_user;
alter  role :telemetry_service_user set search_path to public;

-- Trading domain service
select set_config('ores.cur_user', :'trading_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :trading_service_user with password :'trading_service_password';
grant :service_role to :trading_service_user;
alter  role :trading_service_user set search_path to public;

-- Compute Grid domain service
select set_config('ores.cur_user', :'compute_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :compute_service_user with password :'compute_service_password';
grant :service_role to :compute_service_user;
alter  role :compute_service_user set search_path to public;

-- Synthetic domain service
select set_config('ores.cur_user', :'synthetic_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :synthetic_service_user with password :'synthetic_service_password';
grant :service_role to :synthetic_service_user;
alter  role :synthetic_service_user set search_path to public;

-- Workflow Orchestration domain service
select set_config('ores.cur_user', :'workflow_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :workflow_service_user with password :'workflow_service_password';
grant :service_role to :workflow_service_user;
alter  role :workflow_service_user set search_path to public;

-- ORE Import domain service
select set_config('ores.cur_user', :'ore_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :ore_service_user with password :'ore_service_password';
grant :service_role to :ore_service_user;
alter  role :ore_service_user set search_path to public;

-- Market Data domain service
select set_config('ores.cur_user', :'marketdata_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :marketdata_service_user with password :'marketdata_service_password';
grant :service_role to :marketdata_service_user;
alter  role :marketdata_service_user set search_path to public;

-- Service Controller domain service
select set_config('ores.cur_user', :'controller_service_user', false);
do $$ begin
    if not exists (select 1 from pg_roles where rolname = current_setting('ores.cur_user')) then
        execute format('create user %I', current_setting('ores.cur_user')); end if; end $$;
alter  user :controller_service_user with password :'controller_service_password';
grant :service_role to :controller_service_user;
alter  role :controller_service_user set search_path to public;

