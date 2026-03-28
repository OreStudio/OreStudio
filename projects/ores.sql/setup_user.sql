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
 * ORES Role-Based User Setup
 *
 * Creates the ORES database roles and users with appropriate permissions.
 * All role names are parameterised so that each environment (local1, local2,
 * etc.) gets fully isolated cluster-level roles.
 *
 * USAGE:
 *   psql -U postgres \
 *     -v owner_role='ores_local2_owner' \
 *     -v rw_role='ores_local2_rw' \
 *     -v ro_role='ores_local2_ro' \
 *     -v service_role='ores_local2_service' \
 *     -v ddl_user='ores_local2_ddl_user' \
 *     -v cli_user='ores_local2_cli_user' \
 *     -v wt_user='ores_local2_wt_user' \
 *     -v comms_user='ores_local2_comms_user' \
 *     -v http_user='ores_local2_http_user' \
 *     -v test_ddl_user='ores_local2_test_ddl_user' \
 *     -v test_dml_user='ores_local2_test_dml_user' \
 *     -v readonly_user='ores_local2_readonly_user' \
 *     -v iam_service_user='ores_local2_iam_service' \
 *     -v refdata_service_user='ores_local2_refdata_service' \
 *     -v dq_service_user='ores_local2_dq_service' \
 *     -v variability_service_user='ores_local2_variability_service' \
 *     -v assets_service_user='ores_local2_assets_service' \
 *     -v synthetic_service_user='ores_local2_synthetic_service' \
 *     -v scheduler_service_user='ores_local2_scheduler_service' \
 *     -v reporting_service_user='ores_local2_reporting_service' \
 *     -v telemetry_service_user='ores_local2_telemetry_service' \
 *     -v trading_service_user='ores_local2_trading_service' \
 *     -v compute_service_user='ores_local2_compute_service' \
 *     -v ddl_password='DDL_PASSWORD' \
 *     -v cli_password='CLI_PASSWORD' \
 *     -v wt_password='WT_PASSWORD' \
 *     -v comms_password='COMMS_PASSWORD' \
 *     -v http_password='HTTP_PASSWORD' \
 *     -v test_ddl_password='TEST_DDL_PASSWORD' \
 *     -v test_dml_password='TEST_DML_PASSWORD' \
 *     -v ro_password='RO_PASSWORD' \
 *     -v iam_service_password='IAM_SERVICE_PASSWORD' \
 *     ... (one per service) \
 *     -f setup_user.sql
 *
 * NEXT STEPS:
 *   1. create_database.sql   - Create database (postgres)
 *   2. setup_schema.sql      - Setup schema (ddl_user)
 */

\set ON_ERROR_STOP on

-- Validate that required role name variables were provided
\if :{?owner_role}
\else
    \echo 'ERROR: owner_role variable is required.'
    \quit
\endif

\if :{?rw_role}
\else
    \echo 'ERROR: rw_role variable is required.'
    \quit
\endif

\if :{?ro_role}
\else
    \echo 'ERROR: ro_role variable is required.'
    \quit
\endif

\if :{?service_role}
\else
    \echo 'ERROR: service_role variable is required.'
    \quit
\endif

\if :{?ddl_user}
\else
    \echo 'ERROR: ddl_user variable is required.'
    \quit
\endif

\if :{?ddl_password}
\else
    \echo 'ERROR: ddl_password variable is required for DDL operations.'
    \quit
\endif

\if :{?cli_user}
\else
    \echo 'ERROR: cli_user variable is required.'
    \quit
\endif

\if :{?cli_password}
\else
    \echo 'ERROR: cli_password variable is required for CLI service.'
    \quit
\endif

\if :{?wt_user}
\else
    \echo 'ERROR: wt_user variable is required.'
    \quit
\endif

\if :{?wt_password}
\else
    \echo 'ERROR: wt_password variable is required for Web Toolkit service.'
    \quit
\endif

\if :{?comms_user}
\else
    \echo 'ERROR: comms_user variable is required.'
    \quit
\endif

\if :{?comms_password}
\else
    \echo 'ERROR: comms_password variable is required for Communications service.'
    \quit
\endif

\if :{?http_user}
\else
    \echo 'ERROR: http_user variable is required.'
    \quit
\endif

\if :{?http_password}
\else
    \echo 'ERROR: http_password variable is required for HTTP service.'
    \quit
\endif

\if :{?test_ddl_user}
\else
    \echo 'ERROR: test_ddl_user variable is required.'
    \quit
\endif

\if :{?test_ddl_password}
\else
    \echo 'ERROR: test_ddl_password variable is required for test DDL operations.'
    \quit
\endif

\if :{?test_dml_user}
\else
    \echo 'ERROR: test_dml_user variable is required.'
    \quit
\endif

\if :{?test_dml_password}
\else
    \echo 'ERROR: test_dml_password variable is required for test DML operations.'
    \quit
\endif

\if :{?readonly_user}
\else
    \echo 'ERROR: readonly_user variable is required.'
    \quit
\endif

\if :{?ro_password}
\else
    \echo 'ERROR: ro_password variable is required for read-only access.'
    \quit
\endif

\if :{?iam_service_user}
\else
    \echo 'ERROR: iam_service_user variable is required.'
    \quit
\endif

\if :{?iam_service_password}
\else
    \echo 'ERROR: iam_service_password variable is required for IAM domain service.'
    \quit
\endif

\if :{?refdata_service_user}
\else
    \echo 'ERROR: refdata_service_user variable is required.'
    \quit
\endif

\if :{?refdata_service_password}
\else
    \echo 'ERROR: refdata_service_password variable is required for Reference Data domain service.'
    \quit
\endif

\if :{?dq_service_user}
\else
    \echo 'ERROR: dq_service_user variable is required.'
    \quit
\endif

\if :{?dq_service_password}
\else
    \echo 'ERROR: dq_service_password variable is required for Data Quality domain service.'
    \quit
\endif

\if :{?variability_service_user}
\else
    \echo 'ERROR: variability_service_user variable is required.'
    \quit
\endif

\if :{?variability_service_password}
\else
    \echo 'ERROR: variability_service_password variable is required for Variability domain service.'
    \quit
\endif

\if :{?assets_service_user}
\else
    \echo 'ERROR: assets_service_user variable is required.'
    \quit
\endif

\if :{?assets_service_password}
\else
    \echo 'ERROR: assets_service_password variable is required for Assets domain service.'
    \quit
\endif

\if :{?synthetic_service_user}
\else
    \echo 'ERROR: synthetic_service_user variable is required.'
    \quit
\endif

\if :{?synthetic_service_password}
\else
    \echo 'ERROR: synthetic_service_password variable is required for Synthetic domain service.'
    \quit
\endif

\if :{?scheduler_service_user}
\else
    \echo 'ERROR: scheduler_service_user variable is required.'
    \quit
\endif

\if :{?scheduler_service_password}
\else
    \echo 'ERROR: scheduler_service_password variable is required for Scheduler domain service.'
    \quit
\endif

\if :{?reporting_service_user}
\else
    \echo 'ERROR: reporting_service_user variable is required.'
    \quit
\endif

\if :{?reporting_service_password}
\else
    \echo 'ERROR: reporting_service_password variable is required for Reporting domain service.'
    \quit
\endif

\if :{?telemetry_service_user}
\else
    \echo 'ERROR: telemetry_service_user variable is required.'
    \quit
\endif

\if :{?telemetry_service_password}
\else
    \echo 'ERROR: telemetry_service_password variable is required for Telemetry domain service.'
    \quit
\endif

\if :{?trading_service_user}
\else
    \echo 'ERROR: trading_service_user variable is required.'
    \quit
\endif

\if :{?trading_service_password}
\else
    \echo 'ERROR: trading_service_password variable is required for Trading domain service.'
    \quit
\endif

\if :{?compute_service_user}
\else
    \echo 'ERROR: compute_service_user variable is required.'
    \quit
\endif

\if :{?compute_service_password}
\else
    \echo 'ERROR: compute_service_password variable is required for Compute domain service.'
    \quit
\endif

-- 1. Create group roles (no login)
-- These act as permission templates
create role :owner_role   nologin;
create role :rw_role      nologin;
create role :ro_role      nologin;
-- service_role: infrastructure-level group for all domain service users.
-- Grants CONNECT on the database and USAGE on the public schema; individual
-- table-level DML is handled by iam_service_db_grants_create.sql.
create role :service_role nologin;

-- 2. Create service users (with login)
-- Application-layer users retain broad rw_role access.
create user :ddl_user      with password :'ddl_password'      in role :owner_role;
create user :cli_user      with password :'cli_password'      in role :rw_role;
create user :wt_user       with password :'wt_password'       in role :rw_role;
create user :comms_user    with password :'comms_password'    in role :rw_role;
create user :http_user     with password :'http_password'     in role :rw_role;
-- Domain service users: no broad role — specific table GRANTs applied in
-- setup_schema.sql via create/iam/iam_service_db_grants_create.sql.
create user :iam_service_user         with password :'iam_service_password';
create user :refdata_service_user     with password :'refdata_service_password';
create user :dq_service_user          with password :'dq_service_password';
create user :variability_service_user with password :'variability_service_password';
create user :assets_service_user      with password :'assets_service_password';
create user :synthetic_service_user   with password :'synthetic_service_password';
create user :scheduler_service_user   with password :'scheduler_service_password';
create user :reporting_service_user   with password :'reporting_service_password';
create user :telemetry_service_user   with password :'telemetry_service_password';
create user :trading_service_user     with password :'trading_service_password';
create user :compute_service_user     with password :'compute_service_password';

-- Add all domain service users to service_role for infrastructure-level grants
grant :service_role to
    :iam_service_user,
    :refdata_service_user,
    :dq_service_user,
    :variability_service_user,
    :assets_service_user,
    :synthetic_service_user,
    :scheduler_service_user,
    :reporting_service_user,
    :telemetry_service_user,
    :trading_service_user,
    :compute_service_user;
create user :test_ddl_user with password :'test_ddl_password' in role :owner_role createdb;
create user :test_dml_user with password :'test_dml_password' in role :rw_role;
create user :readonly_user with password :'ro_password'       in role :ro_role;

-- Set default search_path for all users
alter role :ddl_user              set search_path to public;
alter role :cli_user              set search_path to public;
alter role :wt_user               set search_path to public;
alter role :comms_user            set search_path to public;
alter role :iam_service_user      set search_path to public;
alter role :refdata_service_user  set search_path to public;
alter role :dq_service_user       set search_path to public;
alter role :variability_service_user set search_path to public;
alter role :assets_service_user   set search_path to public;
alter role :synthetic_service_user set search_path to public;
alter role :scheduler_service_user set search_path to public;
alter role :reporting_service_user set search_path to public;
alter role :telemetry_service_user set search_path to public;
alter role :trading_service_user  set search_path to public;
alter role :compute_service_user  set search_path to public;
alter role :http_user             set search_path to public;
alter role :test_ddl_user         set search_path to public;
alter role :test_dml_user         set search_path to public;
alter role :readonly_user         set search_path to public;

-- Set default tenant context to system tenant for test users.
alter role :test_ddl_user set app.current_tenant_id = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
alter role :test_dml_user set app.current_tenant_id = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

-- Grant pg_cron permissions to application users (if pg_cron is installed).
do $$
declare
    v_comms_user           text := :'comms_user';
    v_iam_service_user     text := :'iam_service_user';
    v_refdata_service_user text := :'refdata_service_user';
    v_dq_service_user      text := :'dq_service_user';
    v_variability_service_user text := :'variability_service_user';
    v_assets_service_user  text := :'assets_service_user';
    v_synthetic_service_user text := :'synthetic_service_user';
    v_scheduler_service_user text := :'scheduler_service_user';
    v_reporting_service_user text := :'reporting_service_user';
    v_telemetry_service_user text := :'telemetry_service_user';
    v_trading_service_user text := :'trading_service_user';
begin
    if exists (select 1 from pg_extension where extname = 'pg_cron') then
        for v_user in select * from (values
            (v_comms_user), (v_iam_service_user), (v_refdata_service_user),
            (v_dq_service_user), (v_variability_service_user), (v_assets_service_user),
            (v_synthetic_service_user), (v_scheduler_service_user),
            (v_reporting_service_user), (v_telemetry_service_user),
            (v_trading_service_user)
        ) as t(u) loop
            execute format('grant connect on database postgres to %I', v_user.u);
            execute format('grant usage on schema cron to %I', v_user.u);
            execute format(
                'grant execute on function cron.schedule_in_database(text,text,text,text,text,boolean) to %I',
                v_user.u);
            execute format('grant execute on function cron.unschedule(text) to %I', v_user.u);
            raise notice 'Granted pg_cron permissions to %', v_user.u;
        end loop;
    else
        raise notice 'pg_cron not installed; skipping pg_cron grants';
        raise notice '(Run setup_extensions.sql after installing pg_cron, then re-run this script)';
    end if;
end $$;

\echo ''
\echo '=========================================='
\echo 'ORES role-based users created successfully!'
\echo '=========================================='
\echo ''
\echo 'Next step: Create database'
\echo '  psql -U postgres -v db_name=''my_database'' -f create_database.sql'
\echo ''
