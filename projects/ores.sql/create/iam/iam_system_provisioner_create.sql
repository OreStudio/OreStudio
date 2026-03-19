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

-- =============================================================================
-- System Provisioner
--
-- Creates the initial system administrator account during bootstrap. This
-- function is called once per deployment to complete the system-level bootstrap
-- flow initiated by the SystemProvisionerWizard.
--
-- The caller (C++ bootstrap_handler) hashes the password before calling this
-- function, since pgcrypto is not available in all deployments.
--
-- Steps performed:
--   1. Create user account with the supplied pre-hashed password
--   2. Create login tracking entry (login_info)
--   3. Assign SuperAdmin role
--   4. Associate with the system party
--   5. Exit bootstrap mode (clear system.bootstrap_mode flag)
--
-- Prerequisites:
--   - Must be called in system tenant context
--   - SuperAdmin role must already be seeded (via iam_roles_populate.sql)
--   - System party must already exist (via refdata_system_party_populate.sql)
--   - system.bootstrap_mode feature flag must exist (via variability_system_flags_populate.sql)
--
-- Parameters:
--   p_principal:     Username for the new admin account
--   p_email:         Email address for the new admin account
--   p_password_hash: Pre-hashed password (Argon2/bcrypt hash from C++ layer)
--   p_actor:         Optional audit actor; defaults to current session actor
--
-- Returns: The UUID of the newly created account
-- =============================================================================
create or replace function ores_iam_create_initial_admin_fn(
    p_principal text,
    p_email text,
    p_password_hash text,
    p_actor text default null
) returns uuid as $$
declare
    v_actor text;
    v_system_tenant_id uuid;
    v_account_id uuid;
    v_super_admin_role_id uuid;
    v_system_party_id uuid;
begin
    v_actor := coalesce(nullif(p_actor, ''), ores_iam_current_actor_fn(), current_user);
    v_system_tenant_id := ores_iam_system_tenant_id_fn();

    -- Verify system tenant context
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception 'Must be in system tenant context to create initial admin. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    if p_principal is null or p_principal = '' then
        raise exception 'Principal cannot be empty' using errcode = '22023';
    end if;
    if p_email is null or p_email = '' then
        raise exception 'Email cannot be empty' using errcode = '22023';
    end if;
    if p_password_hash is null or p_password_hash = '' then
        raise exception 'Password hash cannot be empty' using errcode = '22023';
    end if;

    v_account_id := gen_random_uuid();

    -- =========================================================================
    -- Create user account
    -- =========================================================================
    insert into ores_iam_accounts_tbl (
        id, tenant_id, version, account_type, username,
        password_hash, password_salt, totp_secret, email,
        modified_by, performed_by, change_reason_code, change_commentary,
        valid_from, valid_to
    ) values (
        v_account_id, v_system_tenant_id, 0, 'user', p_principal,
        p_password_hash, '', '', p_email,
        v_actor, v_actor,
        'system.initial_load', 'Initial system admin created during bootstrap',
        current_timestamp, ores_utility_infinity_timestamp_fn()
    );

    raise notice 'Created account: %', p_principal;

    -- =========================================================================
    -- Create login tracking entry
    -- =========================================================================
    insert into ores_iam_login_info_tbl (
        account_id, last_ip, last_attempt_ip,
        failed_logins, locked, last_login, online
    ) values (
        v_account_id, '0.0.0.0', '0.0.0.0',
        0, 0, '1970-01-01 00:00:00+00', 0
    );

    -- =========================================================================
    -- Assign SuperAdmin role
    -- =========================================================================
    select id into v_super_admin_role_id
    from ores_iam_roles_tbl
    where tenant_id = v_system_tenant_id
      and name = 'SuperAdmin'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_super_admin_role_id is null then
        raise exception 'SuperAdmin role not found. Ensure RBAC is properly seeded.'
            using errcode = '23503';
    end if;

    insert into ores_iam_account_roles_tbl (
        tenant_id, account_id, role_id, assigned_by,
        change_reason_code, change_commentary,
        valid_from, valid_to
    ) values (
        v_system_tenant_id, v_account_id, v_super_admin_role_id, v_actor,
        'system.initial_load', 'SuperAdmin role assigned during bootstrap',
        current_timestamp, ores_utility_infinity_timestamp_fn()
    );

    raise notice 'Assigned SuperAdmin role to: %', p_principal;

    -- =========================================================================
    -- Associate with system party
    -- =========================================================================
    v_system_party_id := ores_iam_account_parties_system_party_id_fn(v_system_tenant_id);

    if v_system_party_id is null then
        raise warning 'No system party found for system tenant; admin account has no party context';
    else
        insert into ores_iam_account_parties_tbl (
            account_id, tenant_id, party_id, version,
            modified_by, performed_by, change_reason_code, change_commentary,
            valid_from, valid_to
        ) values (
            v_account_id, v_system_tenant_id, v_system_party_id, 0,
            v_actor, v_actor,
            'system.initial_load', 'System admin associated with system party during bootstrap',
            current_timestamp, ores_utility_infinity_timestamp_fn()
        );

        raise notice 'Associated % with system party', p_principal;
    end if;

    -- =========================================================================
    -- Exit bootstrap mode
    -- =========================================================================
    perform ores_variability_feature_flags_upsert_fn(
        v_system_tenant_id,
        'system.bootstrap_mode',
        false,
        'Bootstrap mode disabled - system now in secure mode'
    );

    raise notice 'Exited bootstrap mode';
    raise notice 'Initial admin creation complete: % (id: %)', p_principal, v_account_id;

    return v_account_id;
end;
$$ language plpgsql security definer;
