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

/**
 * IAM Admin Utilities
 *
 * Helper functions for administrative tasks such as generating shell commands
 * for role assignments, account management, etc.
 */

-- =============================================================================
-- Role Assignment Command Generator
-- =============================================================================

/**
 * Generate ores.shell commands to assign all roles to an account.
 *
 * This utility function helps administrators by generating the shell commands
 * needed to assign roles to a user account. It can look up accounts by either
 * hostname or tenant_id.
 *
 * @param p_username  The username of the account
 * @param p_hostname  The hostname to identify the tenant (optional if tenant_id provided)
 * @param p_tenant_id The tenant UUID directly (optional if hostname provided)
 *
 * Usage examples:
 *   -- By hostname
 *   SELECT * FROM ores_iam_generate_role_commands_fn('username', 'example.com');
 *
 *   -- By tenant_id
 *   SELECT * FROM ores_iam_generate_role_commands_fn('username', p_tenant_id := 'uuid');
 */
create or replace function ores_iam_generate_role_commands_fn(
    p_username text,
    p_hostname text default null,
    p_tenant_id uuid default null
)
returns table (command text) as $$
declare
    v_account_id uuid;
    v_tenant_id uuid;
    v_resolved_hostname text;
begin
    -- Resolve tenant_id from hostname or use provided tenant_id
    -- Note: tenants.id is the actual tenant identifier, not tenants.tenant_id
    -- (tenant_id in tenants table is always system tenant since it owns the records)
    if p_tenant_id is not null then
        v_tenant_id := p_tenant_id;
        -- Get hostname for display
        select hostname into v_resolved_hostname
        from ores_iam_tenants_tbl
        where id = v_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
        limit 1;
    elsif p_hostname is not null then
        select id, hostname into v_tenant_id, v_resolved_hostname
        from ores_iam_tenants_tbl
        where hostname = p_hostname
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_tenant_id is null then
            return query select '# ERROR: Hostname not found: ' || p_hostname;
            return;
        end if;
    else
        return query select '# ERROR: Must provide either hostname or tenant_id';
        return;
    end if;

    -- Find account ID for the given username and tenant
    select a.id into v_account_id
    from ores_iam_accounts_tbl a
    where a.username = p_username
      and a.tenant_id = v_tenant_id
      and a.valid_to = ores_utility_infinity_timestamp_fn();

    if v_account_id is null then
        return query select '# ERROR: Account not found: ' || p_username || '@' ||
            coalesce(v_resolved_hostname, v_tenant_id::text);
        return;
    end if;

    -- Header with account info
    return query select '# Role assignment commands for: ' || p_username || '@' ||
        coalesce(v_resolved_hostname, v_tenant_id::text);
    return query select '# Account ID: ' || v_account_id::text;
    return query select '';

    -- Generate commands for each role with description as comment
    return query
    select format('accounts assign-role %s %s  # %s', v_account_id, r.id, r.name)
    from ores_iam_roles_tbl r
    where r.valid_to = ores_utility_infinity_timestamp_fn()
    order by r.name;
end;
$$ language plpgsql security definer;

comment on function ores_iam_generate_role_commands_fn(text, text, uuid) is
'Generate ores.shell commands to assign all roles to an account.
Usage:
  SELECT * FROM ores_iam_generate_role_commands_fn(''username'', ''hostname'');
  SELECT * FROM ores_iam_generate_role_commands_fn(''username'', p_tenant_id := ''uuid'');';
