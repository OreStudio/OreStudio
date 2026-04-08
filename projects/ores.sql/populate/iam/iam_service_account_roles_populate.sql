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
 * Template: sql_service_account_roles_populate.mustache
 *
 * Service Account Role Assignments
 *
 * Assigns each domain service account its corresponding NATS RBAC role.
 * Must run after iam_service_accounts_populate.sql and iam_roles_populate.sql.
 *
 * This script is idempotent.
 */

\echo '--- Service Account Role Assignments ---'

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'iam_service_user', 'IamService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'refdata_service_user', 'RefdataService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'dq_service_user', 'DqService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'variability_service_user', 'VariabilityService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'assets_service_user', 'AssetsService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'scheduler_service_user', 'SchedulerService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'reporting_service_user', 'ReportingService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'telemetry_service_user', 'TelemetryService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'trading_service_user', 'TradingService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'compute_service_user', 'ComputeService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'synthetic_service_user', 'SyntheticService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'workflow_service_user', 'WorkflowService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'ore_service_user', 'OreService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'marketdata_service_user', 'MarketdataService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'controller_service_user', 'ControllerService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'analytics_service_user', 'AnalyticsService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'http_user', 'HttpService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'wt_user', 'WtService');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'compute_wrapper_user', 'ComputeWrapperService');

-- Summary
select 'Service Account Role Assignments' as entity, count(*) as count
from ores_iam_account_roles_tbl ar
join ores_iam_accounts_tbl a on a.id = ar.account_id
where a.account_type != 'user'
  and ar.valid_to = ores_utility_infinity_timestamp_fn();
