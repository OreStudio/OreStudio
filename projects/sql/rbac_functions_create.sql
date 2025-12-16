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
set schema 'oresdb';

--
-- RBAC query functions for efficient batch operations.
--

-- Get all permission codes for an account via role assignments.
-- Single query with JOINs avoiding N+1 problem.
create or replace function get_effective_permissions(p_account_id uuid)
returns table(code text) as $$
begin
    return query
    select distinct p.code
    from oresdb.permissions p
    join oresdb.role_permissions rp on p.id = rp.permission_id
    join oresdb.account_roles ar on rp.role_id = ar.role_id
    where ar.account_id = p_account_id
    and p.valid_to = '9999-12-31 23:59:59'::timestamptz
    and rp.valid_to = '9999-12-31 23:59:59'::timestamptz
    and ar.valid_to = '9999-12-31 23:59:59'::timestamptz
    order by p.code;
end;
$$ language plpgsql stable;

-- Get all role-permission mappings with permission codes.
-- Used for batch loading permissions when listing all roles.
create or replace function get_all_role_permission_codes()
returns table(role_id text, code text) as $$
begin
    return query
    select rp.role_id::text, p.code
    from oresdb.role_permissions rp
    join oresdb.permissions p on rp.permission_id = p.id
    where rp.valid_to = '9999-12-31 23:59:59'::timestamptz
    and p.valid_to = '9999-12-31 23:59:59'::timestamptz
    order by rp.role_id, p.code;
end;
$$ language plpgsql stable;

-- Get permission codes for specific roles.
-- More efficient than get_all_role_permission_codes when only a few roles needed.
create or replace function get_role_permission_codes(p_role_ids uuid[])
returns table(role_id text, code text) as $$
begin
    return query
    select rp.role_id::text, p.code
    from oresdb.role_permissions rp
    join oresdb.permissions p on rp.permission_id = p.id
    where rp.role_id = any(p_role_ids)
    and rp.valid_to = '9999-12-31 23:59:59'::timestamptz
    and p.valid_to = '9999-12-31 23:59:59'::timestamptz
    order by rp.role_id, p.code;
end;
$$ language plpgsql stable;

-- Get roles by their IDs in a single query.
-- Avoids N+1 when fetching multiple roles.
create or replace function get_roles_by_ids(p_role_ids uuid[])
returns table(
    id uuid,
    version integer,
    name text,
    description text,
    modified_by text
) as $$
begin
    return query
    select r.id, r.version, r.name, r.description, r.modified_by
    from oresdb.roles r
    where r.id = any(p_role_ids)
    and r.valid_to = '9999-12-31 23:59:59'::timestamptz
    order by r.name;
end;
$$ language plpgsql stable;

-- Get all roles assigned to an account with their permission codes.
-- Returns one row per role with permissions as a comma-separated string.
-- This allows fetching all role data in a single query.
create or replace function get_account_roles_with_permissions(p_account_id uuid)
returns table(
    role_id uuid,
    role_version integer,
    role_name text,
    role_description text,
    role_modified_by text,
    permission_codes text
) as $$
begin
    return query
    select
        r.id,
        r.version,
        r.name,
        r.description,
        r.modified_by,
        coalesce(string_agg(p.code, ',' order by p.code), '') as permission_codes
    from oresdb.account_roles ar
    join oresdb.roles r on ar.role_id = r.id
    left join oresdb.role_permissions rp on r.id = rp.role_id
        and rp.valid_to = '9999-12-31 23:59:59'::timestamptz
    left join oresdb.permissions p on rp.permission_id = p.id
        and p.valid_to = '9999-12-31 23:59:59'::timestamptz
    where ar.account_id = p_account_id
    and ar.valid_to = '9999-12-31 23:59:59'::timestamptz
    and r.valid_to = '9999-12-31 23:59:59'::timestamptz
    group by r.id, r.version, r.name, r.description, r.modified_by
    order by r.name;
end;
$$ language plpgsql stable;
