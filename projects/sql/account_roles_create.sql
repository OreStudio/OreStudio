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
-- Account-Role junction table for RBAC.
-- Links accounts to roles in a many-to-many relationship.
-- Uses composite primary key (account_id, role_id, valid_from).
--
create table if not exists "oresdb"."account_roles" (
    "account_id" uuid not null,
    "role_id" uuid not null,
    "assigned_by" text not null,
    "assigned_at" timestamp with time zone not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (account_id, role_id, valid_from),
    exclude using gist (
        account_id WITH =,
        role_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for looking up roles by account
create index if not exists account_roles_account_idx
on "oresdb"."account_roles" (account_id)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

-- Index for looking up accounts by role
create index if not exists account_roles_role_idx
on "oresdb"."account_roles" (role_id)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

create or replace function update_account_roles()
returns trigger as $$
begin
    -- Close any existing record with this account_id/role_id combination
    update "oresdb"."account_roles"
    set valid_to = current_timestamp
    where account_id = new.account_id
    and role_id = new.role_id
    and valid_to = '9999-12-31 23:59:59'::timestamptz
    and valid_from < current_timestamp;

    new.valid_from = current_timestamp;
    new.valid_to = '9999-12-31 23:59:59'::timestamptz;
    new.assigned_at = current_timestamp;
    if new.assigned_by is null or new.assigned_by = '' then
        new.assigned_by = current_user;
    end if;

    return new;
end;
$$ language plpgsql;

create or replace trigger update_account_roles_trigger
before insert on "oresdb"."account_roles"
for each row
execute function update_account_roles();
