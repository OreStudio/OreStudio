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
-- Permissions table for RBAC.
-- Permissions are seed data and rarely change, so we use simple temporal support.
--
create table if not exists "oresdb"."permissions" (
    "id" uuid not null,
    "code" text not null,
    "description" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, valid_from, valid_to),
    exclude using gist (
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Unique constraint on code for current records
create unique index if not exists permissions_code_unique_idx
on "oresdb"."permissions" (code)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

create or replace function update_permissions()
returns trigger as $$
begin
    -- Close any existing record with this id
    update "oresdb"."permissions"
    set valid_to = current_timestamp
    where id = new.id
    and valid_to = '9999-12-31 23:59:59'::timestamptz
    and valid_from < current_timestamp;

    new.valid_from = current_timestamp;
    new.valid_to = '9999-12-31 23:59:59'::timestamptz;

    return new;
end;
$$ language plpgsql;

create or replace trigger update_permissions_trigger
before insert on "oresdb"."permissions"
for each row
execute function update_permissions();

-- Use a RULE instead of a trigger to avoid tuple modification conflicts
-- Rules rewrite the query before execution, so there's no conflict with the DELETE
create or replace rule delete_permissions_rule as
on delete to "oresdb"."permissions"
do instead
  update "oresdb"."permissions"
  set valid_to = current_timestamp
  where id = old.id
  and valid_to = '9999-12-31 23:59:59'::timestamptz;
