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
-- Tenant Activate Function
-- =============================================================================
-- Transitions a tenant from 'bootstrapping' to 'active' once provisioning
-- wizards have completed. Called by the TenantProvisioningWizard via the
-- iam.v1.tenants.complete-provisioning NATS endpoint.
--
-- SECURITY: Caller must supply the authenticated actor's username (validated
-- against ores_iam_accounts_tbl). Function is SECURITY DEFINER so it can
-- write the tenant record from any DB context without an explicit system-tenant
-- grant to the calling service user.
--
-- Parameters:
--   p_tenant_id: UUID of the tenant to activate
--   p_actor:     Username of the account completing provisioning

create or replace function ores_iam_mark_tenant_active_fn(
    p_tenant_id uuid,
    p_actor text
) returns void as $$
begin
    -- Verify tenant exists and is in bootstrapping state
    if not exists (
        select 1 from ores_iam_tenants_tbl
        where id = p_tenant_id
          and status = 'bootstrapping'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise notice 'Tenant % is not in bootstrapping state; nothing to activate.', p_tenant_id;
        return;
    end if;

    -- Insert new temporal version with status = 'active'.
    -- The INSERT trigger handles: soft-deleting the old row, incrementing
    -- version, and setting valid_from / valid_to timestamps.
    insert into ores_iam_tenants_tbl (
        id, type, code, name, description, hostname, status, version,
        modified_by, change_reason_code, change_commentary
    )
    select id, type, code, name, description, hostname, 'active', version,
        p_actor, 'system.new_record',
        'Tenant activated after provisioning wizard completed'
    from ores_iam_tenants_tbl
    where id = p_tenant_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    raise notice 'Tenant % marked as active.', p_tenant_id;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
