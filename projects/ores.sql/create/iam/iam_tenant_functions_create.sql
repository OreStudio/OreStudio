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
-- If p_tenant_id is NULL or system tenant ID, uses the current session tenant_id
-- (System tenant ID is treated as placeholder for "use session tenant")
create or replace function ores_iam_validate_tenant_fn(
    p_tenant_id uuid
) returns uuid as $$
declare
    v_tenant_id uuid;
begin
    -- Use provided tenant_id or fall back to session tenant
    -- Treat system tenant ID (nil UUID) as placeholder meaning "use session tenant"
    if p_tenant_id is not null and p_tenant_id != ores_iam_system_tenant_id_fn() then
        v_tenant_id := p_tenant_id;
    else
        v_tenant_id := ores_iam_current_tenant_id_fn();
    end if;

    -- If still null/system tenant after fallback, accept it for reference data
    -- This allows population scripts running with system tenant session
    if v_tenant_id is null or v_tenant_id = ores_iam_system_tenant_id_fn() then
        return ores_iam_system_tenant_id_fn();
    end if;

    -- Allow during initial bootstrap when tenants table might be empty
    if not exists (select 1 from ores_iam_tenants_tbl limit 1) then
        return v_tenant_id;
    end if;

    if not exists (
        select 1 from ores_iam_tenants_tbl
        where tenant_id = v_tenant_id
        and status = 'active'
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid or inactive tenant_id: %. Tenant must exist and be active.',
            v_tenant_id using errcode = '23503';
    end if;

    return v_tenant_id;
end;
$$ language plpgsql stable;

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

-- Lookup tenant by code (used by CLI and other tools)
create or replace function ores_iam_tenant_by_code_fn(
    p_code text
) returns uuid as $$
declare
    v_tenant_id uuid;
begin
    select tenant_id into v_tenant_id
    from ores_iam_tenants_tbl
    where code = p_code
    and status = 'active'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'No active tenant found for code: %', p_code
            using errcode = '23503';
    end if;

    return v_tenant_id;
end;
$$ language plpgsql;

-- Lookup tenant name by ID (used for display in login responses)
-- Returns 'System' for the system tenant, otherwise looks up the tenant name.
create or replace function ores_iam_tenant_name_by_id_fn(
    p_tenant_id uuid
) returns text as $$
declare
    v_name text;
begin
    -- Return 'System' for the system tenant
    if p_tenant_id = ores_iam_system_tenant_id_fn() then
        return 'System';
    end if;

    select name into v_name
    from ores_iam_tenants_tbl
    where tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'No active tenant found for ID: %', p_tenant_id
            using errcode = '23503';
    end if;

    return v_name;
end;
$$ language plpgsql;

-- Check if current session is in system tenant
create or replace function ores_iam_is_system_tenant_fn()
returns boolean as $$
begin
    return ores_iam_current_tenant_id_fn() = ores_iam_system_tenant_id_fn();
end;
$$ language plpgsql stable;

-- Set tenant context by code pattern (for interactive psql sessions)
-- Usage: SELECT * FROM ores_iam_set_tenant_fn('ores.cli%');
-- Short alias: SELECT * FROM st('ores.cli%');
-- Returns the tenant that was set, or raises an error if no match or multiple matches
create or replace function ores_iam_set_tenant_fn(
    p_code_pattern text
) returns table (
    tenant_id uuid,
    code text,
    name text,
    description text,
    status text
) as $$
declare
    v_count integer;
    v_tenant_id uuid;
begin
    -- Count matching active tenants
    select count(*) into v_count
    from ores_iam_tenants_tbl t
    where t.code like p_code_pattern
    and t.valid_to = ores_utility_infinity_timestamp_fn();

    if v_count = 0 then
        raise exception 'No tenant found matching pattern: %', p_code_pattern;
    elsif v_count > 1 then
        raise exception 'Multiple tenants (%) match pattern: %. Be more specific.',
            v_count, p_code_pattern;
    end if;

    -- Get the tenant_id and set it
    select t.tenant_id into v_tenant_id
    from ores_iam_tenants_tbl t
    where t.code like p_code_pattern
    and t.valid_to = ores_utility_infinity_timestamp_fn();

    perform set_config('app.current_tenant_id', v_tenant_id::text, false);

    -- Return the tenant info
    return query
    select t.tenant_id, t.code, t.name, t.description, t.status
    from ores_iam_tenants_tbl t
    where t.tenant_id = v_tenant_id
    and t.valid_to = ores_utility_infinity_timestamp_fn();
end;
$$ language plpgsql;

-- Short alias for ores_iam_set_tenant_fn (convenience for interactive use)
-- Usage: SELECT * FROM st('ores.cli%');
create or replace function st(p_code_pattern text)
returns table (
    tenant_id uuid,
    code text,
    name text,
    description text,
    status text
) as $$
    select * from ores_iam_set_tenant_fn(p_code_pattern);
$$ language sql;

-- Generic trigger function to set tenant_id from session variable on insert.
-- Used by tables that don't have their own complex insert triggers.
create or replace function ores_iam_set_tenant_id_on_insert_fn()
returns trigger as $$
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);
    return new;
end;
$$ language plpgsql;

-- Read all latest tenants including soft-deleted ones.
-- Returns the most recent version of each tenant ordered by name.
-- Used by admin interfaces that need to see all tenants regardless of deletion status.
create or replace function ores_iam_read_all_latest_tenants_fn()
returns table (
    id uuid,
    tenant_id uuid,
    version integer,
    type text,
    code text,
    name text,
    description text,
    hostname text,
    status text,
    modified_by text,
    change_reason_code text,
    change_commentary text,
    valid_from timestamp without time zone,
    valid_to timestamp without time zone
) as $$
begin
    return query
    select distinct on (t.id)
        t.id,
        t.tenant_id,
        t.version,
        t.type,
        t.code,
        t.name,
        t.description,
        t.hostname,
        t.status,
        t.modified_by,
        t.change_reason_code,
        t.change_commentary,
        t.valid_from,
        t.valid_to
    from ores_iam_tenants_tbl t
    order by t.id, t.valid_from desc;
end;
$$ language plpgsql security definer;
