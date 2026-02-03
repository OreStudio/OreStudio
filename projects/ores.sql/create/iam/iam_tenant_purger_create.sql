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
-- Tenant Purge Functions
-- =============================================================================
-- These functions permanently delete tenant data (hard delete).
-- Use with caution - data cannot be recovered after purging.

-- -----------------------------------------------------------------------------
-- Purge a single tenant (hard delete all data)
-- -----------------------------------------------------------------------------
-- Permanently deletes all data for a tenant including the tenant record itself.
-- The system tenant cannot be purged.
-- Caller must have system tenant context set.
--
-- Parameters:
--   p_tenant_id: The tenant ID to purge
--
create or replace function ores_iam_purge_tenant_fn(
    p_tenant_id uuid
) returns void as $$
declare
    v_system_tenant_id uuid;
    v_tenant_code text;
    v_deleted_count integer;
begin
    v_system_tenant_id := ores_iam_system_tenant_id_fn();

    -- Cannot purge system tenant
    if p_tenant_id = v_system_tenant_id then
        raise exception 'Cannot purge the system tenant' using errcode = '42501';
    end if;

    -- Verify we're in system tenant context
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception 'Must be in system tenant context to purge a tenant. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    -- Get tenant code for logging (check both active and terminated tenants)
    select code into v_tenant_code
    from ores_iam_tenants_tbl
    where id = p_tenant_id
    limit 1;

    if not found then
        raise exception 'Tenant not found: %', p_tenant_id using errcode = '23503';
    end if;

    raise notice 'Purging tenant: % (id: %)', v_tenant_code, p_tenant_id;

    -- =========================================================================
    -- Hard delete all tenant data
    -- Order matters for foreign key relationships (delete children first)
    -- =========================================================================

    -- IAM: Sessions
    delete from ores_iam_sessions_tbl where tenant_id = p_tenant_id;
    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Deleted % sessions', v_deleted_count;
    end if;

    -- IAM: Login info
    delete from ores_iam_login_info_tbl where tenant_id = p_tenant_id;
    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Deleted % login info records', v_deleted_count;
    end if;

    -- IAM: Account roles (FK to accounts and roles)
    delete from ores_iam_account_roles_tbl where tenant_id = p_tenant_id;
    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Deleted % account-role assignments', v_deleted_count;
    end if;

    -- IAM: Role permissions (FK to roles and permissions)
    delete from ores_iam_role_permissions_tbl where tenant_id = p_tenant_id;
    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Deleted % role-permission assignments', v_deleted_count;
    end if;

    -- IAM: Accounts
    delete from ores_iam_accounts_tbl where tenant_id = p_tenant_id;
    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Deleted % accounts', v_deleted_count;
    end if;

    -- IAM: Roles
    delete from ores_iam_roles_tbl where tenant_id = p_tenant_id;
    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Deleted % roles', v_deleted_count;
    end if;

    -- IAM: Permissions
    delete from ores_iam_permissions_tbl where tenant_id = p_tenant_id;
    get diagnostics v_deleted_count = row_count;
    if v_deleted_count > 0 then
        raise notice 'Deleted % permissions', v_deleted_count;
    end if;

    -- =========================================================================
    -- Delete refdata
    -- =========================================================================

    delete from ores_refdata_currencies_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_countries_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_account_types_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_asset_classes_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_asset_measures_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_benchmark_rates_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_business_centres_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_business_processes_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_cashflow_types_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_entity_classifications_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_local_jurisdictions_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_party_relationships_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_party_roles_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_person_roles_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_regulatory_corporate_sectors_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_reporting_regimes_tbl where tenant_id = p_tenant_id;
    delete from ores_refdata_supervisory_bodies_tbl where tenant_id = p_tenant_id;

    raise notice 'Deleted all refdata for tenant';

    -- =========================================================================
    -- Delete other tenant data
    -- =========================================================================

    -- Assets: Image tags (FK to images and tags)
    delete from ores_assets_image_tags_tbl where tenant_id = p_tenant_id;

    -- Assets: Tags
    delete from ores_assets_tags_tbl where tenant_id = p_tenant_id;

    -- Assets: Images
    delete from ores_assets_images_tbl where tenant_id = p_tenant_id;

    -- Variability: Feature flags
    delete from ores_variability_feature_flags_tbl where tenant_id = p_tenant_id;

    raise notice 'Tenant data purge complete: % (id: %)', v_tenant_code, p_tenant_id;
end;
$$ language plpgsql;

-- -----------------------------------------------------------------------------
-- Hard delete tenant records (bypasses soft-delete rule)
-- -----------------------------------------------------------------------------
-- Called after ores_iam_purge_tenant_fn to remove the tenant record itself.
-- Must be called separately since we need to disable/enable the delete rule.
--
create or replace function ores_iam_hard_delete_tenants_fn(
    p_tenant_ids uuid[]
) returns integer as $$
declare
    v_count integer;
begin
    -- Disable the soft-delete rule
    alter table ores_iam_tenants_tbl disable rule ores_iam_tenants_delete_rule;

    -- Hard delete all specified tenants (by their id, not tenant_id which is always system)
    delete from ores_iam_tenants_tbl where id = any(p_tenant_ids);
    get diagnostics v_count = row_count;

    -- Re-enable the soft-delete rule
    alter table ores_iam_tenants_tbl enable rule ores_iam_tenants_delete_rule;

    return v_count;
end;
$$ language plpgsql;

-- -----------------------------------------------------------------------------
-- Purge all test tenants
-- -----------------------------------------------------------------------------
-- Permanently deletes all tenants whose code starts with 'ores.' (test tenants).
-- This is useful for cleaning up after test runs.
--
-- Returns the number of tenants purged.
--
create or replace function ores_iam_purge_test_tenants_fn()
returns integer as $$
declare
    v_tenant record;
    v_tenant_ids uuid[];
    v_count integer := 0;
begin
    -- Ensure we're in system tenant context
    perform set_config('app.current_tenant_id',
        ores_iam_system_tenant_id_fn()::text, false);

    raise notice 'Purging all test tenants...';

    -- First collect all test tenant IDs and purge their data
    -- Matches: ores.* (new format) and test_* (old format)
    for v_tenant in
        select distinct id, code
        from ores_iam_tenants_tbl
        where code like 'ores.%' or code like 'test_%'
        order by code
    loop
        perform ores_iam_purge_tenant_fn(v_tenant.id);
        v_tenant_ids := array_append(v_tenant_ids, v_tenant.id);
        v_count := v_count + 1;
    end loop;

    -- Now hard-delete the tenant records (requires disabling the soft-delete rule)
    if v_count > 0 then
        perform ores_iam_hard_delete_tenants_fn(v_tenant_ids);
    end if;

    raise notice 'Purged % test tenants', v_count;
    return v_count;
end;
$$ language plpgsql;
