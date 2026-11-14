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
-- These functions handle the complete lifecycle of tenant creation: the tenant
-- record, the credentials its members start with, and the rows it is seeded
-- with.

-- -----------------------------------------------------------------------------
-- Provision a new tenant
-- -----------------------------------------------------------------------------
-- Creates a new tenant and seeds it with two kinds of row, and no third:
--
--   1. The tenant's own authorization rows, copied from the system tenant and
--      rewritten for the new tenant: permissions, roles, role-permission
--      assignments and role-product-type grants. A tenant's roles are its own,
--      because its members are granted them; they are copied so a new tenant
--      offers the roles its starting point does.
--   2. The rows the bundle map makes due when a tenant is created, which
--      ores_dq_tenant_bootstrap_fn publishes. That function names the bundles in
--      one place, so this script holds no list to keep in step.
--
-- Everything else a tenant reads is one row set in the system tenant, read
-- there rather than copied: the DQ registries, the trading lookups, the series
-- classification rules and every reference vocabulary. A shared row a tenant
-- could write would admit two values for one fact, which is the defect the rule
-- removes. Transactional reference data (currencies, countries, parties) is
-- neither copied nor published here: tenants populate it from the dataset
-- library.
--
-- The system tenant must exist and be active.
-- Caller must have system tenant context set.
--
-- Parameters:
--   p_type: Tenant type ('production', 'evaluation', or 'automation')
--   p_code: Unique tenant code (e.g., 'acme', 'test_20260201_143052_abc')
--   p_name: Display name for the tenant
--   p_hostname: Unique hostname (e.g., 'acme.example.com', 'test_123.localhost')
--   p_description: Optional description
--
-- Returns: (tenant_id uuid, system_party_id uuid)
--
create or replace function ores_iam_provision_tenant_fn(
    p_type        text,
    p_code        text,
    p_name        text,
    p_hostname    text,
    p_description text default null,
    p_actor       text default null,
    out tenant_id      uuid,
    out system_party_id uuid
) as $$
declare
    v_system_tenant_id uuid;
    v_tenant_id        uuid;
    v_copied_count     integer;
    v_actor            text;
    v_owner_id         uuid;
    v_boot             record;
begin
    -- Resolve actor: use explicit parameter if provided, otherwise fall back to
    -- session GUC (ores_iam_current_actor_fn), then current_user.
    -- current_user is always ores_ddl_user inside SECURITY DEFINER functions.
    v_actor := coalesce(nullif(p_actor, ''), ores_iam_current_actor_fn(), current_user);

    -- Get system tenant ID
    v_system_tenant_id := ores_utility_system_tenant_id_fn();

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
    tenant_id := gen_random_uuid();
    v_tenant_id := tenant_id;

    -- Create the tenant record
    -- Note: The trigger sets tenant_id = system_tenant_id (all tenants owned by system)
    insert into ores_iam_tenants_tbl (
        id, type, code, name, description, hostname, status,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        tenant_id, p_type, p_code, p_name, p_description, p_hostname, 'bootstrapping',
        v_actor, v_actor, 'system.new_record', 'Tenant provisioned'
    );

    raise notice 'Created tenant: % (id: %)', p_code, tenant_id;

    -- NOTE: We keep system tenant context during data copying.
    -- The SELECTs need system tenant context to read source data (due to RLS).
    -- The INSERTs explicitly specify tenant_id, which the trigger validates.

    -- =========================================================================
    -- Copy IAM data from system tenant
    -- =========================================================================

    -- Copy permissions
    insert into ores_iam_permissions_tbl (id, tenant_id, code, description, valid_from, valid_to)
    select gen_random_uuid(), v_tenant_id, p.code, p.description,
           current_timestamp, ores_utility_infinity_timestamp_fn()
    from ores_iam_permissions_tbl p
    where p.tenant_id = v_system_tenant_id
    and p.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % permissions', v_copied_count;

    -- Copy roles (versioned table with audit columns). The askable flag comes
    -- across, so a provisioned tenant offers its members the same roles the
    -- starting point does and no others.
    insert into ores_iam_roles_tbl (
        id, tenant_id, name, description, is_requestable,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        gen_random_uuid(), v_tenant_id, r.name, r.description, r.is_requestable,
        v_actor, v_actor, 'system.new_record', 'Copied from system tenant during provisioning'
    from ores_iam_roles_tbl r
    where r.tenant_id = v_system_tenant_id
    and r.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % roles', v_copied_count;

    -- Copy role_permissions. Need to map old role/permission IDs to new IDs
    -- by name/code. The copy states the same tail the roles copy does, so a
    -- provisioned tenant's bundle rows say where they came from.
    -- A version of 0 states no earlier version to match: the trigger numbers the row.
    insert into ores_iam_role_permissions_tbl (
        tenant_id, role_id, permission_id, version,
        modified_by, performed_by,
        change_reason_code, change_commentary)
    select
        v_tenant_id,
        new_r.id,
        new_p.id,
        0,
        v_actor, v_actor,
        'system.new_record', 'Copied from system tenant during provisioning'
    from ores_iam_role_permissions_tbl rp
    join ores_iam_roles_tbl old_r on old_r.id = rp.role_id
        and old_r.tenant_id = v_system_tenant_id
        and old_r.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_permissions_tbl old_p on old_p.id = rp.permission_id
        and old_p.tenant_id = v_system_tenant_id
        and old_p.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_roles_tbl new_r on new_r.name = old_r.name
        and new_r.tenant_id = v_tenant_id
        and new_r.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_permissions_tbl new_p on new_p.code = old_p.code
        and new_p.tenant_id = v_tenant_id
        and new_p.valid_to = ores_utility_infinity_timestamp_fn()
    where rp.tenant_id = v_system_tenant_id
    and rp.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % role-permission assignments', v_copied_count;

    -- Give every copied role the whole product type catalogue. The junction
    -- is the grant, so a role with no rows permits nothing, and a tenant that
    -- could not book what the system catalogue states would be provisioned
    -- unable to trade. The catalogue is the one copy in the system tenant, and
    -- it is read, not copied: a product type added to it is bookable by the
    -- next tenant provisioned without this script changing.
    insert into ores_iam_role_product_types_tbl (
        tenant_id, role_id, product_type_code, version,
        modified_by, performed_by,
        change_reason_code, change_commentary)
    select
        v_tenant_id,
        r.id,
        c.code,
        0,
        v_actor, v_actor,
        'system.new_record', 'The system catalogue copied during provisioning'
    from ores_iam_roles_tbl r
    cross join ores_trading_product_types_tbl c
    where r.tenant_id = v_tenant_id
      and r.valid_to = ores_utility_infinity_timestamp_fn()
      and c.tenant_id = v_system_tenant_id
      and c.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_copied_count = row_count;
    raise notice 'Copied % role-product type grants', v_copied_count;

    -- =========================================================================
    -- Seed the tenant's own rows from the bundle map
    -- =========================================================================
    -- A new tenant is seeded with the bundles the bundle map places at the
    -- tenant level and makes due when the tenant is created. The bootstrap
    -- function names them in one place, so this script holds no list to keep in
    -- step with the map.
    --
    -- Nothing else is copied. The DQ registries, the trading lookups, the series
    -- classification rules and the reference vocabularies are one row set in the
    -- system tenant, and every tenant reads them there. The system party below
    -- is written by this script and not by a dataset: it is the tenant's own
    -- row, and no publish function writes one.

    for v_boot in select * from ores_dq_tenant_bootstrap_fn(v_tenant_id) loop
        raise notice 'Tenant bootstrap: % % row(s) from dataset %',
            v_boot.action, v_boot.record_count, v_boot.dataset_code;
    end loop;

    -- =========================================================================
    -- Create the system party for the new tenant
    -- =========================================================================
    -- Every tenant gets exactly one system party (party_category='System')
    -- which serves as the root of the party hierarchy. The system party
    -- represents the tenant organisation itself. It is the only party with
    -- parent_party_id = NULL, enforced by a partial unique index.

    -- Seed the WRLD business centre for the new tenant so that the system
    -- party's business_center_code passes validation if later edited via UI.
    insert into ores_refdata_business_centres_tbl (
        code, tenant_id, version, coding_scheme_code,
        description, modified_by, performed_by,
        change_reason_code, change_commentary
    ) values (
        'WRLD', v_tenant_id, 0, 'NONE',
        'World. Global business centre for entities not tied to a specific geographic location.',
        v_actor, v_actor,
        'system.new_record', 'System business centre for tenant'
    );

    raise notice 'Created WRLD business centre for tenant: %', p_code;

    system_party_id := gen_random_uuid();
    insert into ores_refdata_parties_tbl (
        id, tenant_id, full_name, short_code, party_category,
        party_type, business_center_code, parent_party_id, status,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        system_party_id, v_tenant_id,
        'System Party', p_code || '_system', 'System',
        'Internal', 'WRLD', null, 'Active',
        v_actor, v_actor, 'system.new_record',
        'System party created during tenant provisioning'
    );

    raise notice 'Created system party for tenant: %', p_code;

    -- =========================================================================
    -- Seed bootstrap mode flag for the new tenant
    -- =========================================================================
    -- New tenants start in bootstrap mode so the tenant provisioning wizard
    -- appears on first login. The wizard clears this flag on completion.

    perform ores_variability_system_settings_upsert_fn(
        v_tenant_id,
        'system.bootstrap_mode',
        'true',
        'boolean',
        'Indicates whether the tenant is in bootstrap mode (waiting for initial setup).'
    );

    -- Seed the tenant onboarding-completion flag (system-party-scoped,
    -- p_party_id omitted). Cleared by the tenant provisioner wizard on
    -- completion via the same NATS-routed pattern as the bootstrap_mode
    -- clear — see ores.iam.core/messaging/tenant_handler.hpp.
    perform ores_variability_system_settings_upsert_fn(
        v_tenant_id,
        'onboarding.tenant',
        'false',
        'boolean',
        'Whether the tenant provisioner wizard has completed for this tenant.'
    );

    -- System settings are tenant-scoped (system_settings_service.get_all()
    -- reads only the caller's tenant), so a "system."-prefixed setting
    -- meant to be a global default must still be seeded per-tenant here —
    -- otherwise SettingGatedActionController silently hides the feature
    -- for every tenant except the system tenant. Copies every current
    -- system.* policy setting from the system tenant (bootstrap_mode
    -- excluded — seeded explicitly above, it's per-tenant state, not a
    -- copied policy default) so a newly-added setting is automatically
    -- seeded for future tenants without a second list to remember.
    perform ores_variability_seed_tenant_system_settings_fn(v_tenant_id);

    raise notice 'Seeded system settings for tenant: %', p_code;

    -- =========================================================================
    -- Provisioning complete
    -- =========================================================================
    -- NOTE: Reference data (currencies, countries, etc.) is NOT copied here.
    -- Tenants populate refdata via the dataset library using the "Publish
    -- Datasets" feature in the Data Librarian.

    raise notice 'Tenant provisioning complete: % (id: %)', p_code, tenant_id;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
