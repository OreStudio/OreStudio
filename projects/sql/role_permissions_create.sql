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
-- Role-Permission junction table for RBAC.
-- Links roles to permissions in a many-to-many relationship.
-- Uses composite primary key (role_id, permission_id, valid_from).
--
create table if not exists "oresdb"."role_permissions" (
    "role_id" uuid not null,
    "permission_id" uuid not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (role_id, permission_id, valid_from),
    exclude using gist (
        role_id WITH =,
        permission_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for looking up permissions by role
create index if not exists role_permissions_role_idx
on "oresdb"."role_permissions" (role_id)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

-- Index for looking up roles by permission
create index if not exists role_permissions_permission_idx
on "oresdb"."role_permissions" (permission_id)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

create or replace function update_role_permissions()
returns trigger as $$
begin
    -- Close any existing record with this role_id/permission_id combination
    update "oresdb"."role_permissions"
    set valid_to = current_timestamp
    where role_id = new.role_id
    and permission_id = new.permission_id
    and valid_to = '9999-12-31 23:59:59'::timestamptz
    and valid_from < current_timestamp;

    new.valid_from = current_timestamp;
    new.valid_to = '9999-12-31 23:59:59'::timestamptz;

    return new;
end;
$$ language plpgsql;

create or replace trigger update_role_permissions_trigger
before insert on "oresdb"."role_permissions"
for each row
execute function update_role_permissions();
