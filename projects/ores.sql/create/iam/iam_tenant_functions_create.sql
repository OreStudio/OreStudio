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
-- Tenant Validation and Helper Functions
-- =============================================================================

-- System tenant ID constant (UUID v7 with all zeros for easy identification)
create or replace function ores_iam_system_tenant_id_fn()
returns uuid as $$
begin
    return '00000000-0000-0000-0000-000000000000'::uuid;
end;
$$ language plpgsql immutable;

-- Get current tenant ID from session variable
create or replace function ores_iam_current_tenant_id_fn()
returns uuid as $$
begin
    return current_setting('app.current_tenant_id', true)::uuid;
exception
    when others then
        return null;
end;
$$ language plpgsql stable;

-- Validate that a tenant_id exists and is active
-- Returns the tenant_id if valid, raises exception otherwise
create or replace function ores_iam_validate_tenant_fn(
    p_tenant_id uuid
) returns uuid as $$
begin
    if p_tenant_id is null then
        raise exception 'tenant_id cannot be null' using errcode = '23502';
    end if;

    -- Allow during initial bootstrap when tenants table might be empty
    if not exists (select 1 from ores_iam_tenants_tbl limit 1) then
        return p_tenant_id;
    end if;

    if not exists (
        select 1 from ores_iam_tenants_tbl
        where tenant_id = p_tenant_id
        and status = 'active'
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid or inactive tenant_id: %. Tenant must exist and be active.',
            p_tenant_id using errcode = '23503';
    end if;

    return p_tenant_id;
end;
$$ language plpgsql;

-- Lookup tenant by hostname (used during login)
create or replace function ores_iam_tenant_by_hostname_fn(
    p_hostname text
) returns uuid as $$
declare
    v_tenant_id uuid;
begin
    select tenant_id into v_tenant_id
    from ores_iam_tenants_tbl
    where hostname = p_hostname
    and status = 'active'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'No active tenant found for hostname: %', p_hostname
            using errcode = '23503';
    end if;

    return v_tenant_id;
end;
$$ language plpgsql;

-- Check if current session is in system tenant
create or replace function ores_iam_is_system_tenant_fn()
returns boolean as $$
begin
    return ores_iam_current_tenant_id_fn() = ores_iam_system_tenant_id_fn();
end;
$$ language plpgsql stable;
