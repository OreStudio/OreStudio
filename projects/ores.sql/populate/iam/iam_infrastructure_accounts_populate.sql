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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * Infrastructure Account Population Script
 *
 * Creates service accounts and assigns IAM roles for infrastructure processes
 * that are NOT managed by the service registry codegen (i.e. they do not
 * appear in ores_services_service_registry.json).
 *
 * Covered accounts:
 *   - compute_wrapper_user  (Compute Wrapper worker — ComputeWrapperService role)
 *   - http_user             (HTTP REST API server  — HttpService role)
 *   - wt_user               (Wt web application   — WtService role)
 *
 * NOTE: http_user and wt_user are created with passwords by the generated
 * iam_service_accounts_populate.sql. Only their role assignments live here.
 * compute_wrapper_user has no DB password; its account is created below.
 *
 * This script is idempotent. It must run after:
 *   - iam_service_accounts_populate.sql  (http_user, wt_user already exist)
 *   - iam_service_account_roles_populate.sql
 *   - iam_roles_populate.sql             (HttpService, WtService,
 *                                         ComputeWrapperService already exist)
 */

SET "ores.compute_wrapper_user" = :'compute_wrapper_user';
SET "ores.http_user"            = :'http_user';
SET "ores.wt_user"             = :'wt_user';

DO $$
BEGIN
    -- --- Infrastructure Account Role Assignments ---

    -- compute_wrapper_user: no DB password — authenticates via session at startup.
    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.compute_wrapper_user'),
        'compute_wrapper@system.ores',
        'System service account for Compute Wrapper worker service'
    );

    PERFORM ores_iam_account_role_assign_fn(
        ores_utility_system_tenant_id_fn(), current_setting('ores.compute_wrapper_user'), 'ComputeWrapperService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_utility_system_tenant_id_fn(), current_setting('ores.http_user'), 'HttpService');

    PERFORM ores_iam_account_role_assign_fn(
        ores_utility_system_tenant_id_fn(), current_setting('ores.wt_user'), 'WtService');
END $$;


-- Summary
select 'Infrastructure Account Roles' as entity, count(*) as count
from ores_iam_account_roles_tbl ar
join ores_iam_accounts_tbl a on a.id = ar.account_id
where a.email in (
    'compute_wrapper@system.ores',
    'http@system.ores',
    'wt@system.ores'
)
  and ar.valid_to = ores_utility_infinity_timestamp_fn();
