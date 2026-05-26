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
-- System Bootstrap Reset Function
-- =============================================================================
-- Returns the entire system to the state right after setup_database.sh ran:
-- all non-system tenants are purged, the system admin accounts are removed,
-- and system.bootstrap_mode is flipped back to 'true' so the
-- SystemProvisionerWizard fires on the next application startup.
--
-- Intended solely for development iteration. Do NOT run against production.
--
-- Steps performed:
--   1. Purge all non-system tenants (hard-delete — cannot be undone)
--   2. End active sessions for the system tenant
--   3. Soft-delete system admin accounts, account-party associations, and
--      account-role assignments
--   4. Delete login-info records for system accounts (non-temporal)
--   5. Re-enable system bootstrap mode (system.bootstrap_mode = 'true')
--
-- What is preserved after reset:
--   - System tenant record
--   - Seeded roles, permissions, reference data
--   - Database schema and all stored functions
--
-- SECURITY: Caller must have system tenant context set.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- System bootstrap reset
-- -----------------------------------------------------------------------------
create or replace function ores_iam_reset_system_fn() returns void as $$
declare
    v_system_tenant_id uuid;
    v_tenant           record;
    v_tenant_ids       uuid[];
    v_affected_count   integer;
    v_total_tenants    integer := 0;
begin
    v_system_tenant_id := ores_utility_system_tenant_id_fn();

    -- Verify we're in system tenant context
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception
            'Must be in system tenant context to reset the system. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    raise notice 'System reset initiated — returning to pre-bootstrap state.';

    -- =========================================================================
    -- Step 1: Purge all non-system tenants (hard delete)
    -- =========================================================================
    raise notice 'Purging all non-system tenants...';

    for v_tenant in
        select distinct id, code
        from ores_iam_tenants_tbl
        where id != v_system_tenant_id
        order by code
    loop
        raise notice '  Purging tenant: % (id: %)', v_tenant.code, v_tenant.id;
        perform ores_iam_purge_tenant_fn(v_tenant.id);
        v_tenant_ids := array_append(v_tenant_ids, v_tenant.id);
        v_total_tenants := v_total_tenants + 1;
    end loop;

    if v_total_tenants > 0 then
        perform ores_iam_hard_delete_tenants_fn(v_tenant_ids);
        raise notice 'Purged and hard-deleted % non-system tenant(s)', v_total_tenants;
    else
        raise notice 'No non-system tenants found';
    end if;

    -- =========================================================================
    -- Step 2: End active sessions for the system tenant
    -- =========================================================================
    update ores_iam_sessions_tbl
    set end_time = ores_utility_iso8601_timestamp_fn(current_timestamp)
    where tenant_id = v_system_tenant_id
      and end_time = '';

    get diagnostics v_affected_count = row_count;
    if v_affected_count > 0 then
        raise notice 'Ended % active session(s)', v_affected_count;
    end if;

    -- =========================================================================
    -- Step 3: Soft-delete system admin accounts and related records
    -- =========================================================================

    -- Account-party associations
    update ores_iam_account_parties_tbl
    set valid_to = current_timestamp
    where tenant_id = v_system_tenant_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_affected_count = row_count;
    if v_affected_count > 0 then
        raise notice 'Soft-deleted % account-party association(s)', v_affected_count;
    end if;

    -- Account-role assignments
    update ores_iam_account_roles_tbl
    set valid_to = current_timestamp
    where tenant_id = v_system_tenant_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_affected_count = row_count;
    if v_affected_count > 0 then
        raise notice 'Soft-deleted % account-role assignment(s)', v_affected_count;
    end if;

    -- Login info (non-temporal; hard delete)
    delete from ores_iam_login_info_tbl
    where account_id in (
        select id
        from ores_iam_accounts_tbl
        where tenant_id = v_system_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    );

    get diagnostics v_affected_count = row_count;
    if v_affected_count > 0 then
        raise notice 'Deleted % login-info record(s)', v_affected_count;
    end if;

    -- Admin accounts (user-type accounts only; service accounts are untouched)
    update ores_iam_accounts_tbl
    set valid_to = current_timestamp
    where tenant_id = v_system_tenant_id
      and account_type = 'user'
      and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_affected_count = row_count;
    if v_affected_count > 0 then
        raise notice 'Soft-deleted % admin account(s)', v_affected_count;
    end if;

    -- =========================================================================
    -- Step 4: Re-enable system bootstrap mode
    -- =========================================================================
    perform ores_variability_system_settings_set_fn(
        v_system_tenant_id,
        'system.bootstrap_mode',
        'true',
        'boolean',
        'Indicates whether the system is in bootstrap mode (waiting for initial admin account).',
        current_user,
        'system.admin_reset',
        'Bootstrap mode re-enabled by dev reset - SystemProvisionerWizard will fire on next startup'
    );

    raise notice 'System bootstrap mode re-enabled (system.bootstrap_mode = true)';
    raise notice 'System reset complete. Start the application to trigger SystemProvisionerWizard.';
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
