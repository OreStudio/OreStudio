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
create schema if not exists oresdb;
create extension if not exists btree_gist;
set schema 'oresdb';

create table if not exists "oresdb"."feature_flags" (
    "name" text not null,
    "enabled" integer not null default 0,
    "description" text,
    "modified_by" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (name, valid_from, valid_to),
    exclude using gist (
        name WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

create or replace function update_feature_flags()
returns trigger as $$
begin
    update "oresdb"."feature_flags"
    set valid_to = current_timestamp
    where name = new.name
    and valid_to = '9999-12-31 23:59:59'::timestamptz
    and valid_from < current_timestamp;

    new.valid_from = current_timestamp;
    new.valid_to = '9999-12-31 23:59:59'::timestamptz;
    new.modified_by = current_user;

    return new;
end;
$$ language plpgsql;

create or replace trigger update_feature_flags_trigger
before insert on "oresdb"."feature_flags"
for each row
execute function update_feature_flags();

-- insert into feature_flags (name, enabled, description)
-- values
--     ('system.security.secure_mode', 0, 'If true, the system enforces full authentication and authorisation.')
-- on conflict (name, tstzrange(valid_from, valid_to)) do nothing;
