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
-- Tenant Provisioning Functions
-- =============================================================================
-- These functions handle the complete lifecycle of tenant creation,
-- including copying required IAM base data from the system tenant.

-- -----------------------------------------------------------------------------
-- Provision a new tenant with IAM base data
-- -----------------------------------------------------------------------------
-- Creates a new tenant and copies only system IAM tables:
--   - permissions: Permission definitions
--   - roles: Role definitions
--   - role_permissions: Role-permission assignments
--
-- Reference data (currencies, countries, etc.) is NOT copied during
-- provisioning. Tenants populate refdata via the dataset library using
-- the "Publish Datasets" feature in the Data Librarian.
--
-- The system tenant must exist and be active.
-- Caller must have system tenant context set.
--
-- Parameters:
--   p_type: Tenant type ('organisation', 'platform', or 'test')
--   p_code: Unique tenant code (e.g., 'acme', 'test_20260201_143052_abc')
--   p_name: Display name for the tenant
--   p_hostname: Unique hostname (e.g., 'acme.example.com', 'test_123.localhost')
--   p_description: Optional description
--
-- Returns: The new tenant_id (UUID)
--
create or replace function ores_iam_provision_tenant_fn(
    p_type text,
    p_code text,
    p_name text,
    p_hostname text,
    p_description text default null
) returns uuid as $$
declare
    v_new_tenant_id uuid;
    v_system_tenant_id uuid;
    v_copied_count integer;
begin
    -- Get system tenant ID
    v_system_tenant_id := ores_iam_system_tenant_id_fn();

    -- Verify we're in system tenant context (required to create tenants)
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception 'Must be in system tenant context to provision a new tenant. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    -- Verify system tenant exists and is active
    if not exists (
        select 1 from ores_iam_tenants_tbl
        where id = v_system_tenant_id
        and status = 'active'
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'System tenant does not exist or is not active. Cannot provision new tenant.'
            using errcode = '23503';
    end if;

    -- Generate new tenant ID
    v_new_tenant_id := gen_random_uuid();

    -- Create the tenant record
    -- Note: The trigger sets tenant_id = system_tenant_id (all tenants owned by system)
    insert into ores_iam_tenants_tbl (
        id, type, code, name, description, hostname, status,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        v_new_tenant_id, p_type, p_code, p_name, p_description, p_hostname, 'active',
        current_user, current_user, 'system.new_record', 'Tenant provisioned'
    );

    raise notice 'Created tenant: % (id: %)', p_code, v_new_tenant_id;

    -- NOTE: We keep system tenant context during data copying.
    -- The SELECTs need system tenant context to read source data (due to RLS).
    -- The INSERTs explicitly specify v_new_tenant_id, which the trigger validates.

    -- =========================================================================
    -- Copy IAM data from system tenant
    -- =========================================================================

    -- Copy permissions (simple table: id, tenant_id, code, description)
    insert into ores_iam_permissions_tbl (id, tenant_id, code, description)
    select gen_random_uuid(), v_new_tenant_id, code, description
    from ores_iam_permissions_tbl
    where tenant_id = v_system_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % permissions', v_copied_count;

    -- Copy roles (versioned table with audit columns)
    insert into ores_iam_roles_tbl (
        id, tenant_id, name, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        gen_random_uuid(), v_new_tenant_id, name, description,
        current_user, current_user, 'system.new_record', 'Copied from system tenant during provisioning'
    from ores_iam_roles_tbl
    where tenant_id = v_system_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % roles', v_copied_count;

    -- Copy role_permissions (simple junction table)
    -- Need to map old role/permission IDs to new IDs by name/code
    insert into ores_iam_role_permissions_tbl (tenant_id, role_id, permission_id)
    select
        v_new_tenant_id,
        new_r.id,
        new_p.id
    from ores_iam_role_permissions_tbl rp
    join ores_iam_roles_tbl old_r on old_r.id = rp.role_id
        and old_r.tenant_id = v_system_tenant_id
        and old_r.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_permissions_tbl old_p on old_p.id = rp.permission_id
        and old_p.tenant_id = v_system_tenant_id
        and old_p.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_roles_tbl new_r on new_r.name = old_r.name
        and new_r.tenant_id = v_new_tenant_id
        and new_r.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_permissions_tbl new_p on new_p.code = old_p.code
        and new_p.tenant_id = v_new_tenant_id
        and new_p.valid_to = ores_utility_infinity_timestamp_fn()
    where rp.tenant_id = v_system_tenant_id
    and rp.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % role-permission assignments', v_copied_count;

    -- =========================================================================
    -- Provisioning complete
    -- =========================================================================
    -- NOTE: Reference data (currencies, countries, etc.) is NOT copied here.
    -- Tenants populate refdata via the dataset library using the "Publish
    -- Datasets" feature in the Data Librarian.

    raise notice 'Tenant provisioning complete: % (id: %)', p_code, v_new_tenant_id;

    return v_new_tenant_id;
end;
$$ language plpgsql;
