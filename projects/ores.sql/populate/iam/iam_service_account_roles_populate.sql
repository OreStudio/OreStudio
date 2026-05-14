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

SET "ores.iam_service_user"         = :'iam_service_user';
SET "ores.refdata_service_user"     = :'refdata_service_user';
SET "ores.dq_service_user"          = :'dq_service_user';
SET "ores.variability_service_user" = :'variability_service_user';
SET "ores.assets_service_user"      = :'assets_service_user';
SET "ores.scheduler_service_user"   = :'scheduler_service_user';
SET "ores.reporting_service_user"   = :'reporting_service_user';
SET "ores.telemetry_service_user"   = :'telemetry_service_user';
SET "ores.trading_service_user"     = :'trading_service_user';
SET "ores.compute_service_user"     = :'compute_service_user';
SET "ores.synthetic_service_user"   = :'synthetic_service_user';
SET "ores.workflow_service_user"    = :'workflow_service_user';
SET "ores.ore_service_user"         = :'ore_service_user';
SET "ores.marketdata_service_user"  = :'marketdata_service_user';
SET "ores.controller_service_user"  = :'controller_service_user';
SET "ores.analytics_service_user"   = :'analytics_service_user';

DO $$
BEGIN
    -- --- Service Account Role Assignments ---

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.iam_service_user'), 'IamService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.refdata_service_user'), 'RefdataService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.dq_service_user'), 'DqService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.variability_service_user'), 'VariabilityService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.assets_service_user'), 'AssetsService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.scheduler_service_user'), 'SchedulerService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.reporting_service_user'), 'ReportingService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.telemetry_service_user'), 'TelemetryService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.trading_service_user'), 'TradingService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.compute_service_user'), 'ComputeService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.synthetic_service_user'), 'SyntheticService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.workflow_service_user'), 'WorkflowService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.ore_service_user'), 'OreService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.marketdata_service_user'), 'MarketdataService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.controller_service_user'), 'ControllerService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_iam_system_tenant_id_fn(), current_setting('ores.analytics_service_user'), 'AnalyticsService');
END $$;


-- Summary
select 'Service Account Role Assignments' as entity, count(*) as count
from ores_iam_account_roles_tbl ar
join ores_iam_accounts_tbl a on a.id = ar.account_id
where a.account_type != 'user'
  and ar.valid_to = ores_utility_infinity_timestamp_fn();
