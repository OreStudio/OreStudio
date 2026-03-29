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
 * Service Account Role Assignments
 *
 * Assigns each domain service account its corresponding NATS RBAC role.
 * Must run after iam_service_accounts_populate.sql and iam_roles_populate.sql.
 *
 * Required psql variables (set by setup_database.sh / recreate_database.sh):
 *   :iam_service_user, :refdata_service_user, :dq_service_user,
 *   :variability_service_user, :assets_service_user,
 *   :synthetic_service_user, :scheduler_service_user,
 *   :reporting_service_user, :telemetry_service_user,
 *   :trading_service_user, :compute_service_user
 *
 * This script is idempotent.
 */

\echo '--- Service Account Role Assignments ---'

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'iam_service_user', 'role_iam_service');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'refdata_service_user', 'role_refdata_service');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'dq_service_user', 'role_dq_service');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'variability_service_user', 'role_variability_service');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'assets_service_user', 'role_assets_service');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'synthetic_service_user', 'role_synthetic_service');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'scheduler_service_user', 'role_scheduler_service');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'reporting_service_user', 'role_reporting_service');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'telemetry_service_user', 'role_telemetry_service');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'trading_service_user', 'role_trading_service');

select ores_iam_account_role_assign_fn(
    ores_iam_system_tenant_id_fn(), :'compute_service_user', 'role_compute_service');

-- Summary
select 'Service Account Role Assignments' as entity, count(*) as count
from ores_iam_account_roles_tbl ar
join ores_iam_accounts_tbl a on a.id = ar.account_id
where a.account_type != 'user'
  and ar.valid_to = ores_utility_infinity_timestamp_fn();
