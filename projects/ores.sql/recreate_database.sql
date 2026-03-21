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
 *       -v iam_service_password='...' -v refdata_service_password='...' \
 *       -v dq_service_password='...' -v variability_service_password='...' \
 *       -v assets_service_password='...' -v synthetic_service_password='...' \
 *       -v scheduler_service_password='...' -v reporting_service_password='...' \
 *       -v telemetry_service_password='...' -v trading_service_password='...' \
 *       -v compute_service_password='...' \
 *       -f recreate_database.sql
 *
 * Variables:
 *   :ddl_password               - password for DDL operations
 *   :cli_password               - password for CLI service
 *   :wt_password                - password for Web Toolkit service
 *   :comms_password             - password for Communications service
 *   :http_password              - password for HTTP service
 *   :test_ddl_password          - password for test DDL operations
 *   :test_dml_password          - password for test DML operations
 *   :ro_password                - password for read-only access
 *   :db_name                    - database name to create
 *   :iam_service_password       - password for IAM domain service
 *   :refdata_service_password   - password for Reference Data domain service
 *   :dq_service_password        - password for Data Quality domain service
 *   :variability_service_password - password for Variability domain service
 *   :assets_service_password    - password for Assets domain service
 *   :synthetic_service_password - password for Synthetic domain service
 *   :scheduler_service_password - password for Scheduler domain service
 *   :reporting_service_password - password for Reporting domain service
 *   :telemetry_service_password - password for Telemetry domain service
 *   :trading_service_password   - password for Trading domain service
 *   :compute_service_password   - password for Compute domain service
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

-- IAM domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_iam_service') then
        create user ores_iam_service;
    end if;
end
$$;
alter  user ores_iam_service with password :'iam_service_password';
grant  ores_rw to ores_iam_service;
alter  role ores_iam_service set search_path to public;

-- Reference Data domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_refdata_service') then
        create user ores_refdata_service;
    end if;
end
$$;
alter  user ores_refdata_service with password :'refdata_service_password';
grant  ores_rw to ores_refdata_service;
alter  role ores_refdata_service set search_path to public;

-- Data Quality domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_dq_service') then
        create user ores_dq_service;
    end if;
end
$$;
alter  user ores_dq_service with password :'dq_service_password';
grant  ores_rw to ores_dq_service;
alter  role ores_dq_service set search_path to public;

-- Variability domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_variability_service') then
        create user ores_variability_service;
    end if;
end
$$;
alter  user ores_variability_service with password :'variability_service_password';
grant  ores_rw to ores_variability_service;
alter  role ores_variability_service set search_path to public;

-- Assets domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_assets_service') then
        create user ores_assets_service;
    end if;
end
$$;
alter  user ores_assets_service with password :'assets_service_password';
grant  ores_rw to ores_assets_service;
alter  role ores_assets_service set search_path to public;

-- Synthetic domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_synthetic_service') then
        create user ores_synthetic_service;
    end if;
end
$$;
alter  user ores_synthetic_service with password :'synthetic_service_password';
grant  ores_rw to ores_synthetic_service;
alter  role ores_synthetic_service set search_path to public;

-- Scheduler domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_scheduler_service') then
        create user ores_scheduler_service;
    end if;
end
$$;
alter  user ores_scheduler_service with password :'scheduler_service_password';
grant  ores_rw to ores_scheduler_service;
alter  role ores_scheduler_service set search_path to public;

-- Reporting domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_reporting_service') then
        create user ores_reporting_service;
    end if;
end
$$;
alter  user ores_reporting_service with password :'reporting_service_password';
grant  ores_rw to ores_reporting_service;
alter  role ores_reporting_service set search_path to public;

-- Telemetry domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_telemetry_service') then
        create user ores_telemetry_service;
    end if;
end
$$;
alter  user ores_telemetry_service with password :'telemetry_service_password';
grant  ores_rw to ores_telemetry_service;
alter  role ores_telemetry_service set search_path to public;

-- Trading domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_trading_service') then
        create user ores_trading_service;
    end if;
end
$$;
alter  user ores_trading_service with password :'trading_service_password';
grant  ores_rw to ores_trading_service;
alter  role ores_trading_service set search_path to public;

-- Compute domain service
do $$
begin
    if not exists (select 1 from pg_roles where rolname = 'ores_compute_service') then
        create user ores_compute_service;
    end if;
end
$$;
alter  user ores_compute_service with password :'compute_service_password';
grant  ores_rw to ores_compute_service;
alter  role ores_compute_service set search_path to public;

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


