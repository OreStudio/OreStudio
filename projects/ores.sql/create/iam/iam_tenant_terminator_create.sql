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
-- Tenant Termination Functions
-- =============================================================================
-- These functions handle marking tenants as terminated without deleting data.
-- Use this after unit tests to preserve data for debugging while preventing
-- further use of the tenant.

-- -----------------------------------------------------------------------------
-- Terminate a tenant (mark inactive, preserve all data)
-- -----------------------------------------------------------------------------
-- Marks a tenant as terminated without modifying any data. This is the
-- lightest-weight tenant lifecycle operation - it simply prevents the tenant
-- from being used while preserving all data for analysis and debugging.
--
-- The system tenant cannot be terminated.
-- Caller must have system tenant context set.
--
-- Parameters:
--   p_tenant_id: The tenant ID to terminate
--
create or replace function ores_iam_terminate_tenant_fn(
    p_tenant_id uuid
) returns void as $$
declare
    v_system_tenant_id uuid;
    v_tenant_code text;
begin
    v_system_tenant_id := ores_iam_system_tenant_id_fn();

    -- Cannot terminate system tenant
    if p_tenant_id = v_system_tenant_id then
        raise exception 'Cannot terminate the system tenant' using errcode = '42501';
    end if;

    -- Verify we're in system tenant context
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception 'Must be in system tenant context to terminate a tenant. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    -- Get tenant code for logging
    select code into v_tenant_code
    from ores_iam_tenants_tbl
    where id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'Tenant not found or already terminated: %', p_tenant_id
            using errcode = '23503';
    end if;

    raise notice 'Terminating tenant: % (id: %)', v_tenant_code, p_tenant_id;

    -- Insert a new version with status='terminated'
    -- The insert trigger will close off the current version and create the new one
    insert into ores_iam_tenants_tbl (
        id, type, code, name, description, hostname, status,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        id, type, code, name, description, hostname, 'terminated',
        current_user, current_user, 'system.tenant_terminated', 'Tenant terminated'
    from ores_iam_tenants_tbl
    where id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    raise notice 'Tenant terminated: % (id: %). All data preserved.', v_tenant_code, p_tenant_id;
end;
$$ language plpgsql;
