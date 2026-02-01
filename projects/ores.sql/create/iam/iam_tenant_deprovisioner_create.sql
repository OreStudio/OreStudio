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

-- =============================================================================
-- Tenant Deprovisioning Functions
-- =============================================================================
-- These functions handle tenant termination and cleanup.
-- Data is soft-deleted (valid_to set to current timestamp) to preserve history.

-- -----------------------------------------------------------------------------
-- Deprovision a tenant (soft delete)
-- -----------------------------------------------------------------------------
-- Terminates a tenant by:
--   1. Updating tenant status to 'terminated'
--   2. Soft-deleting all tenant data (sets valid_to to current timestamp)
--
-- The system tenant cannot be deprovisioned.
-- Caller must have system tenant context set.
--
-- Parameters:
--   p_tenant_id: The tenant ID to deprovision
--
create or replace function ores_iam_deprovision_tenant_fn(
    p_tenant_id uuid
) returns void as $$
declare
    v_system_tenant_id uuid;
    v_tenant_code text;
    v_deleted_count integer;
begin
    v_system_tenant_id := ores_iam_system_tenant_id_fn();

    -- Cannot deprovision system tenant
    if p_tenant_id = v_system_tenant_id then
        raise exception 'Cannot deprovision the system tenant' using errcode = '42501';
    end if;

    -- Verify we're in system tenant context
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception 'Must be in system tenant context to deprovision a tenant. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    -- Get tenant code for logging
    select code into v_tenant_code
    from ores_iam_tenants_tbl
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'Tenant not found or already terminated: %', p_tenant_id
            using errcode = '23503';
    end if;

    raise notice 'Deprovisioning tenant: % (id: %)', v_tenant_code, p_tenant_id;

    -- =========================================================================
    -- Soft-delete all tenant data
    -- Order matters for foreign key relationships (delete children first)
    -- =========================================================================

    -- IAM: Sessions (no FK constraints, can be deleted first)
    update ores_iam_sessions_tbl
    set end_time = to_char(current_timestamp, 'YYYY-MM-DD HH24:MI:SS.US')
    where tenant_id = p_tenant_id
    and end_time = '';

    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Ended % active sessions', v_deleted_count;
    end if;

    -- IAM: Login info (delete, no temporal)
    delete from ores_iam_login_info_tbl where tenant_id = p_tenant_id;

    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Deleted % login info records', v_deleted_count;
    end if;

    -- IAM: Account roles (FK to accounts and roles)
    update ores_iam_account_roles_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Soft-deleted % account-role assignments', v_deleted_count;
    end if;

    -- IAM: Role permissions (FK to roles and permissions)
    update ores_iam_role_permissions_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Soft-deleted % role-permission assignments', v_deleted_count;
    end if;

    -- IAM: Accounts
    update ores_iam_accounts_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Soft-deleted % accounts', v_deleted_count;
    end if;

    -- IAM: Roles
    update ores_iam_roles_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Soft-deleted % roles', v_deleted_count;
    end if;

    -- IAM: Permissions
    update ores_iam_permissions_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Soft-deleted % permissions', v_deleted_count;
    end if;

    -- =========================================================================
    -- Soft-delete refdata
    -- =========================================================================

    update ores_refdata_currencies_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_countries_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_account_types_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_asset_classes_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_asset_measures_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_benchmark_rates_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_business_centres_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_business_processes_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_cashflow_types_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_entity_classifications_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_local_jurisdictions_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_party_relationships_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_party_roles_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_person_roles_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_regulatory_corporate_sectors_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_reporting_regimes_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    update ores_refdata_supervisory_bodies_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    raise notice 'Soft-deleted all refdata for tenant';

    -- =========================================================================
    -- Soft-delete other tenant data
    -- =========================================================================

    -- Assets: Image tags (FK to images and tags)
    update ores_assets_image_tags_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    -- Assets: Tags
    update ores_assets_tags_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    -- Assets: Images
    update ores_assets_images_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    -- Variability: Feature flags
    update ores_variability_feature_flags_tbl
    set valid_to = current_timestamp
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    -- =========================================================================
    -- Finally, terminate the tenant itself
    -- Note: We only update status, not valid_to, so the tenant record remains
    -- visible for auditing and debugging purposes.
    -- =========================================================================
    update ores_iam_tenants_tbl
    set status = 'terminated'
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    raise notice 'Tenant deprovisioning complete: % (id: %)', v_tenant_code, p_tenant_id;
end;
$$ language plpgsql;
