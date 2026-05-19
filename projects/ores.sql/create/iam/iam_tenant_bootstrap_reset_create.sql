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

-- =============================================================================
-- Tenant Bootstrap Reset Function
-- =============================================================================
-- Full dev-cycle reset for a tenant: soft-deletes admin-created data AND
-- re-enables bootstrap mode, so both the TenantProvisioningWizard and the
-- PartyProvisioningWizard fire on next login.
--
-- Contrast with ores_iam_reset_tenant_fn (party-level reset only, preserves
-- bootstrap state) and the full tenant lifecycle functions:
--   * reset            -- soft-delete admin-created data only (no wizard re-fire)
--   * bootstrap reset  -- soft-delete admin data + re-enable both wizards (this fn)
--   * deprovision      -- soft-delete ALL temporal data including IAM
--   * purge            -- hard-delete everything
--
-- After this function completes:
--   - All admin-created data is soft-deleted (same as ores_iam_reset_tenant_fn)
--   - Operational parties are Inactive (PartyProvisioningWizard fires on login)
--   - system.bootstrap_mode = 'true' (TenantProvisioningWizard fires on login)
--
-- What is preserved:
--   - Tenant record, admin account, roles, permissions
--   - Parties and account-party associations (login remains functional)
--   - Base reference data (currencies, countries, etc.)
--
-- SECURITY: Caller must have system tenant context set. The system tenant
-- cannot be bootstrap-reset via this function (use ores_iam_reset_system_fn).
--
-- Parameters:
--   p_tenant_code: The tenant code to reset (e.g. 'ores.dev.local3')
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Tenant bootstrap reset
-- -----------------------------------------------------------------------------
create or replace function ores_iam_reset_tenant_bootstrap_fn(
    p_tenant_code text,
    p_actor text default null
) returns void as $$
declare
    v_tenant_id uuid;
    v_system_tenant_id uuid;
    v_actor text;
begin
    v_system_tenant_id := ores_utility_system_tenant_id_fn();
    v_actor := coalesce(nullif(p_actor, ''), current_user);

    -- Verify we're in system tenant context
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception
            'Must be in system tenant context to perform tenant bootstrap reset. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    -- Look up tenant by code
    select id into v_tenant_id
    from ores_iam_tenants_tbl
    where code = p_tenant_code
      and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'Tenant not found or inactive: %', p_tenant_code
            using errcode = '23503';
    end if;

    -- Cannot bootstrap-reset the system tenant
    if v_tenant_id = v_system_tenant_id then
        raise exception 'Cannot bootstrap-reset the system tenant. Use ores_iam_reset_system_fn instead.'
            using errcode = '42501';
    end if;

    raise notice 'Bootstrap-resetting tenant: % (id: %)', p_tenant_code, v_tenant_id;

    -- =========================================================================
    -- Step 1: Party-level reset (soft-delete admin data, parties → Inactive)
    -- =========================================================================
    -- Delegates to ores_iam_reset_tenant_fn which handles:
    --   - Counterparties, identifiers, contacts, party-counterparty links
    --   - Business units, portfolios, books
    --   - Report definitions, scheduler job definitions
    --   - Flip Operational parties back to Inactive (triggers PartyProvisioningWizard)
    perform ores_iam_reset_tenant_fn(v_tenant_id);

    -- =========================================================================
    -- Step 2: Re-enable bootstrap mode (triggers TenantProvisioningWizard)
    -- =========================================================================
    perform ores_variability_system_settings_set_fn(
        v_tenant_id,
        'system.bootstrap_mode',
        'true',
        'boolean',
        'Indicates whether the tenant is in bootstrap mode (waiting for initial setup).',
        current_user,
        'system.admin_reset',
        'Bootstrap mode re-enabled by dev reset - TenantProvisioningWizard will re-fire on next login'
    );

    raise notice 'Bootstrap mode re-enabled for tenant: %', p_tenant_code;

    -- =========================================================================
    -- Step 3: Set tenant status to 'bootstrapping'
    -- =========================================================================
    -- Insert a new temporal version of the tenant record with status='bootstrapping'.
    -- The INSERT trigger handles: soft-deleting the old row, incrementing version,
    -- setting valid_from/valid_to timestamps.
    insert into ores_iam_tenants_tbl (
        id, type, code, name, description, hostname, status, version,
        modified_by, change_reason_code, change_commentary
    )
    select id, type, code, name, description, hostname, 'bootstrapping', version,
        v_actor, 'system.admin_reset',
        'Tenant reset to bootstrap state - provisioning wizards will re-fire on next login'
    from ores_iam_tenants_tbl
    where id = v_tenant_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    raise notice 'Tenant status set to bootstrapping: %', p_tenant_code;
    raise notice 'Tenant bootstrap reset complete: %', p_tenant_code;
    raise notice 'On next login: TenantProvisioningWizard and PartyProvisioningWizard will fire.';
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
