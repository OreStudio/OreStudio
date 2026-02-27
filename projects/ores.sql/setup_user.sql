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
 * This implements a role-based access control system with separate users for
 * different services and operations.
 *
 * USAGE:
 *   psql -U postgres \
 *     -v ddl_password='DDL_PASSWORD' \
 *     -v cli_password='CLI_PASSWORD' \
 *     -v wt_password='WT_PASSWORD' \
 *     -v comms_password='COMMS_PASSWORD' \
 *     -v http_password='HTTP_PASSWORD' \
 *     -v test_ddl_password='TEST_DDL_PASSWORD' \
 *     -v test_dml_password='TEST_DML_PASSWORD' \
 *     -v ro_password='RO_PASSWORD' \
 *     -f setup_user.sql
 *
 * NEXT STEPS:
 *   1. create_database.sql   - Create database (postgres)
 *   2. setup_schema.sql      - Setup schema (ores_ddl_user)
 *
 * NOTE: Generate secure passwords with:
 *   pwgen -c 25 1
 */

\set ON_ERROR_STOP on

-- Validate that required passwords were provided
\if :{?ddl_password}
\else
    \echo 'ERROR: ddl_password variable is required for DDL operations.'
    \echo 'Usage: psql -U postgres -v ddl_password=''YOUR_PASSWORD'' ... -f setup_user.sql'
    \quit
\endif

\if :{?cli_password}
\else
    \echo 'ERROR: cli_password variable is required for CLI service.'
    \quit
\endif

\if :{?wt_password}
\else
    \echo 'ERROR: wt_password variable is required for Web Toolkit service.'
    \quit
\endif

\if :{?comms_password}
\else
    \echo 'ERROR: comms_password variable is required for Communications service.'
    \quit
\endif

\if :{?http_password}
\else
    \echo 'ERROR: http_password variable is required for HTTP service.'
    \quit
\endif

\if :{?test_ddl_password}
\else
    \echo 'ERROR: test_ddl_password variable is required for test DDL operations.'
    \quit
\endif

\if :{?test_dml_password}
\else
    \echo 'ERROR: test_dml_password variable is required for test DML operations.'
    \quit
\endif

\if :{?ro_password}
\else
    \echo 'ERROR: ro_password variable is required for read-only access.'
    \quit
\endif

-- 1. Create group roles (no login)
-- These act as permission templates
create role ores_owner nologin;  -- for ddl/migrations
create role ores_rw    nologin;  -- for application dml (data manipulation)
create role ores_ro    nologin;  -- for analytics/reporting

-- 2. Create service users (with login)
-- Inherit from appropriate roles for standard operations
create user ores_ddl_user      with password :'ddl_password'      in role ores_owner;
create user ores_cli_user      with password :'cli_password'      in role ores_rw;
create user ores_wt_user       with password :'wt_password'       in role ores_rw;
create user ores_comms_user    with password :'comms_password'    in role ores_rw;
create user ores_http_user     with password :'http_password'     in role ores_rw;
create user ores_test_ddl_user with password :'test_ddl_password' in role ores_owner createdb;
create user ores_test_dml_user with password :'test_dml_password' in role ores_rw;

-- optional: read-only user for devs/bi
create user ores_readonly_user with password :'ro_password' in role ores_ro;

-- Set default search_path for all ores users
-- Note: search_path must be set on users, not group roles (it doesn't inherit)
alter role ores_ddl_user set search_path to public;
alter role ores_cli_user set search_path to public;
alter role ores_wt_user set search_path to public;
alter role ores_comms_user set search_path to public;
alter role ores_http_user set search_path to public;
alter role ores_test_ddl_user set search_path to public;
alter role ores_test_dml_user set search_path to public;
alter role ores_readonly_user set search_path to public;

-- Set default tenant context to system tenant for test users.
-- This is essential for test operations that don't explicitly set tenant context.
alter role ores_test_ddl_user set app.current_tenant_id = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
alter role ores_test_dml_user set app.current_tenant_id = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

-- Grant pg_cron permissions to application users (if pg_cron is installed).
-- ores.scheduler uses cron.schedule_in_database() which is called from the
-- postgres database, so the application users need access to the cron schema here.
do $$
begin
    if exists (select 1 from pg_extension where extname = 'pg_cron') then
        grant connect on database postgres to ores_comms_user;
        grant usage on schema cron to ores_comms_user;
        grant execute on function
            cron.schedule_in_database(text, text, text, text, text, boolean)
            to ores_comms_user;
        grant execute on function cron.unschedule(text) to ores_comms_user;
        raise notice 'Granted pg_cron permissions to ores_comms_user';
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
