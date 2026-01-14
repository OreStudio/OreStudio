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
 * Roles Population Script
 *
 * Seeds the RBAC roles table with predefined roles and their permission
 * assignments. This script is idempotent - running it multiple times will
 * not create duplicate entries.
 *
 * Predefined Roles:
 * - Admin: Full access (wildcard permission)
 * - Trading: Currency read access for trading operations
 * - Sales: Read-only currency access for sales
 * - Operations: Currency management and account viewing
 * - Support: Read-only access to all resources
 * - Viewer: Basic read-only access (default role for new accounts)
 *
 * Prerequisites:
 * - permissions_populate.sql must be run first
 */

set schema 'ores';

-- Helper function to create a role if it doesn't exist
create or replace function ores.upsert_role(
    p_name text,
    p_description text,
    p_recorded_by text default 'system'
) returns uuid as $$
declare
    v_id uuid;
begin
    -- Check if role already exists
    select id into v_id
    from ores.iam_roles_tbl
    where name = p_name and valid_to = ores.utility_infinity_timestamp_fn();

    if v_id is null then
        v_id := gen_random_uuid();
        -- The update_roles trigger will set version and valid_from/valid_to
        insert into ores.iam_roles_tbl (id, version, name, description, modified_by,
            change_reason_code, change_commentary, valid_from, valid_to)
        values (v_id, 1, p_name, p_description, p_recorded_by,
                'system.new_record', 'System seed data',
                current_timestamp, ores.utility_infinity_timestamp_fn());
        raise notice 'Created role: %', p_name;
    else
        raise notice 'Role already exists: %', p_name;
    end if;

    return v_id;
end;
$$ language plpgsql;

-- Helper function to assign a permission to a role
create or replace function ores.assign_permission_to_role(
    p_role_name text,
    p_permission_code text,
    p_assigned_by text default 'system'
) returns void as $$
declare
    v_role_id uuid;
    v_permission_id uuid;
begin
    -- Get role ID (roles table uses 'id' column)
    select id into v_role_id
    from ores.iam_roles_tbl
    where name = p_role_name and valid_to = ores.utility_infinity_timestamp_fn();

    if v_role_id is null then
        raise exception 'Role not found: %', p_role_name;
    end if;

    -- Get permission ID (permissions table uses 'id' column)
    select id into v_permission_id
    from ores.iam_permissions_tbl
    where code = p_permission_code and valid_to = ores.utility_infinity_timestamp_fn();

    if v_permission_id is null then
        raise exception 'Permission not found: %', p_permission_code;
    end if;

    -- Check if assignment already exists
    if not exists (
        select 1 from ores.iam_role_permissions_tbl
        where role_id = v_role_id
          and permission_id = v_permission_id
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        -- The update_role_permissions trigger will set valid_from/valid_to
        insert into ores.iam_role_permissions_tbl (role_id, permission_id, valid_from, valid_to)
        values (v_role_id, v_permission_id, current_timestamp, ores.utility_infinity_timestamp_fn());
    end if;
end;
$$ language plpgsql;

-- Create roles
select ores.upsert_role('Admin', 'Full administrative access to all system functions');
select ores.upsert_role('Trading', 'Trading operations - currency read access');
select ores.upsert_role('Sales', 'Sales operations - read-only currency access');
select ores.upsert_role('Operations', 'Operations - currency management and account viewing');
select ores.upsert_role('Support', 'Support - read-only access to all resources and admin screens');
select ores.upsert_role('Viewer', 'Viewer - basic read-only access to domain data');

-- Assign permissions to Admin role (wildcard)
select ores.assign_permission_to_role('Admin', '*');

-- Assign permissions to Trading role
select ores.assign_permission_to_role('Trading', 'currencies:read');
select ores.assign_permission_to_role('Trading', 'currencies:history');
select ores.assign_permission_to_role('Trading', 'flags:read');

-- Assign permissions to Sales role
select ores.assign_permission_to_role('Sales', 'currencies:read');
select ores.assign_permission_to_role('Sales', 'flags:read');

-- Assign permissions to Operations role
select ores.assign_permission_to_role('Operations', 'currencies:create');
select ores.assign_permission_to_role('Operations', 'currencies:read');
select ores.assign_permission_to_role('Operations', 'currencies:update');
select ores.assign_permission_to_role('Operations', 'currencies:delete');
select ores.assign_permission_to_role('Operations', 'currencies:history');
select ores.assign_permission_to_role('Operations', 'flags:read');
select ores.assign_permission_to_role('Operations', 'accounts:read');

-- Assign permissions to Support role
select ores.assign_permission_to_role('Support', 'accounts:read');
select ores.assign_permission_to_role('Support', 'currencies:read');
select ores.assign_permission_to_role('Support', 'currencies:history');
select ores.assign_permission_to_role('Support', 'flags:read');
select ores.assign_permission_to_role('Support', 'login_info:read');
select ores.assign_permission_to_role('Support', 'roles:read');

-- Assign permissions to Viewer role (default for new accounts)
select ores.assign_permission_to_role('Viewer', 'currencies:read');
select ores.assign_permission_to_role('Viewer', 'flags:read');

-- Clean up helper functions
drop function ores.upsert_role(text, text, text);
drop function ores.assign_permission_to_role(text, text, text);

-- Show summary
select 'Roles:' as summary, count(*) as count from ores.iam_roles_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Role-Permission assignments:', count(*) from ores.iam_role_permissions_tbl
where valid_to = ores.utility_infinity_timestamp_fn();
