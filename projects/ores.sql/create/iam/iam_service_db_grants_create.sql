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

-- AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
-- Template: sql_service_db_grants.mustache
--
-- Service Account DB Permission Grants
--
-- Each domain service user receives SELECT/INSERT/UPDATE/DELETE on its own
-- component tables only, plus SELECT on specific cross-component tables it
-- legitimately reads. This replaces the previous broad rw_role membership.
--
-- All tables live in the public schema with the naming convention:
--   ores_<component>_*_tbl
--
-- Uses a temporary helper function to grant DML on all tables matching a
-- prefix, so grants automatically cover new tables added to a component.
--
-- Variables (passed via psql -v from setup_database.sh):
--   One {name}_service_user variable per service (see service_vars.sh).

-- ---------------------------------------------------------------------------
-- Helper: grant DML on every table whose name starts with p_prefix.
-- Dropped at the bottom of this script.
-- ---------------------------------------------------------------------------
create or replace function _ores_grant_dml_fn(p_prefix text, p_user text)
returns void language plpgsql as $$
declare
    v_tbl text;
begin
    for v_tbl in
        select tablename
        from pg_tables
        where schemaname = 'public'
          and tablename like p_prefix || '%'
    loop
        execute format(
            'grant select, insert, update, delete on %I to %I',
            v_tbl, p_user);
    end loop;
end $$;

-- Helper: grant SELECT on every table whose name starts with p_prefix.
create or replace function _ores_grant_select_fn(p_prefix text, p_user text)
returns void language plpgsql as $$
declare
    v_tbl text;
begin
    for v_tbl in
        select tablename
        from pg_tables
        where schemaname = 'public'
          and tablename like p_prefix || '%'
    loop
        execute format('grant select on %I to %I', v_tbl, p_user);
    end loop;
end $$;

-- ---------------------------------------------------------------------------
-- Sequence access — granted to all service users.
-- ---------------------------------------------------------------------------
grant usage, select on all sequences in schema public
    to
    :iam_service_user,
    :refdata_service_user,
    :dq_service_user,
    :variability_service_user,
    :assets_service_user,
    :scheduler_service_user,
    :reporting_service_user,
    :telemetry_service_user,
    :trading_service_user,
    :compute_service_user,
    :synthetic_service_user,
    :workflow_service_user,
    :marketdata_service_user;

alter default privileges in schema public
    grant usage, select on sequences
    to
    :iam_service_user,
    :refdata_service_user,
    :dq_service_user,
    :variability_service_user,
    :assets_service_user,
    :scheduler_service_user,
    :reporting_service_user,
    :telemetry_service_user,
    :trading_service_user,
    :compute_service_user,
    :synthetic_service_user,
    :workflow_service_user,
    :marketdata_service_user;

-- ---------------------------------------------------------------------------
-- Per-service grants
-- ---------------------------------------------------------------------------
-- ---------------------------------------------------------------------------
-- iam_service: IAM domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_iam_', :'iam_service_user');

-- ---------------------------------------------------------------------------
-- refdata_service: Reference Data domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_refdata_', :'refdata_service_user');
grant select on ores_iam_tenants_tbl to :refdata_service_user;

-- ---------------------------------------------------------------------------
-- dq_service: Data Quality domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_dq_', :'dq_service_user');
grant select on ores_iam_tenants_tbl to :dq_service_user;

-- ---------------------------------------------------------------------------
-- variability_service: Variability domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_variability_', :'variability_service_user');
grant select on ores_iam_tenants_tbl to :variability_service_user;

-- ---------------------------------------------------------------------------
-- assets_service: Assets domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_assets_', :'assets_service_user');
grant select on ores_iam_tenants_tbl to :assets_service_user;

-- ---------------------------------------------------------------------------
-- scheduler_service: Scheduler domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_scheduler_', :'scheduler_service_user');
grant select on ores_iam_tenants_tbl to :scheduler_service_user;
grant select on ores_dq_change_reasons_tbl to :scheduler_service_user;
grant select on ores_dq_change_reason_categories_tbl to :scheduler_service_user;

-- ---------------------------------------------------------------------------
-- reporting_service: Reporting domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_reporting_', :'reporting_service_user');
grant select on ores_iam_tenants_tbl to :reporting_service_user;
grant select on ores_dq_change_reasons_tbl to :reporting_service_user;
grant select on ores_dq_change_reason_categories_tbl to :reporting_service_user;
select _ores_grant_select_fn('ores_scheduler_', :'reporting_service_user');

-- ---------------------------------------------------------------------------
-- telemetry_service: Telemetry domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_telemetry_', :'telemetry_service_user');
grant select on ores_iam_tenants_tbl to :telemetry_service_user;

-- ---------------------------------------------------------------------------
-- trading_service: Trading domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_trading_', :'trading_service_user');
grant select on ores_iam_tenants_tbl to :trading_service_user;
grant select on ores_dq_change_reasons_tbl to :trading_service_user;
grant select on ores_dq_change_reason_categories_tbl to :trading_service_user;
select _ores_grant_select_fn('ores_refdata_', :'trading_service_user');

-- ---------------------------------------------------------------------------
-- compute_service: Compute Grid domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_compute_', :'compute_service_user');
grant select on ores_iam_tenants_tbl to :compute_service_user;
grant select on ores_refdata_parties_tbl to :compute_service_user;

-- ---------------------------------------------------------------------------
-- synthetic_service: Synthetic domain service
-- ---------------------------------------------------------------------------
select _ores_grant_select_fn('ores_iam_', :'synthetic_service_user');
select _ores_grant_select_fn('ores_refdata_', :'synthetic_service_user');
select _ores_grant_select_fn('ores_dq_', :'synthetic_service_user');
select _ores_grant_select_fn('ores_trading_', :'synthetic_service_user');
select _ores_grant_select_fn('ores_variability_', :'synthetic_service_user');
select _ores_grant_select_fn('ores_assets_', :'synthetic_service_user');
select _ores_grant_select_fn('ores_reporting_', :'synthetic_service_user');
select _ores_grant_select_fn('ores_scheduler_', :'synthetic_service_user');
select _ores_grant_select_fn('ores_compute_', :'synthetic_service_user');
select _ores_grant_select_fn('ores_telemetry_', :'synthetic_service_user');

-- ---------------------------------------------------------------------------
-- workflow_service: Workflow Orchestration domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_workflow_', :'workflow_service_user');
select _ores_grant_dml_fn('ores_iam_', :'workflow_service_user');
select _ores_grant_dml_fn('ores_refdata_parties', :'workflow_service_user');
grant select on ores_iam_tenants_tbl to :workflow_service_user;

-- ---------------------------------------------------------------------------
-- marketdata_service: Market Data domain service
-- ---------------------------------------------------------------------------
select _ores_grant_dml_fn('ores_marketdata_', :'marketdata_service_user');
grant select on ores_iam_tenants_tbl to :marketdata_service_user;

-- ---------------------------------------------------------------------------
-- Clean up helper functions
-- ---------------------------------------------------------------------------
drop function _ores_grant_dml_fn(text, text);
drop function _ores_grant_select_fn(text, text);
