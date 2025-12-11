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

-- Feature Flags Table Schema
-- Component: ores.variability
-- Purpose: Stores system feature flags with bitemporal validity tracking

create schema if not exists oresdb;
create extension if not exists btree_gist;
set schema 'oresdb';

create table if not exists "oresdb"."feature_flags" (
    "name" text not null,
    "version" integer not null,
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

-- Unique constraint on version for current records ensures version uniqueness per entity
create unique index if not exists feature_flags_version_unique_idx
on "oresdb"."feature_flags" (name, version)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

create or replace function update_feature_flags()
returns trigger as $$
declare
    current_version integer;
begin
    -- Get the current version of the existing record (if any)
    select version into current_version
    from "oresdb"."feature_flags"
    where name = new.name
    and valid_to = '9999-12-31 23:59:59'::timestamptz;

    if found then
        -- Existing record: check version for optimistic locking
        -- Version 0 is a special "force overwrite" sentinel used by imports
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        -- Increment version for the new record
        new.version = current_version + 1;

        -- Close the existing record
        update "oresdb"."feature_flags"
        set valid_to = current_timestamp
        where name = new.name
        and valid_to = '9999-12-31 23:59:59'::timestamptz
        and valid_from < current_timestamp;
    else
        -- New record: set initial version
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = '9999-12-31 23:59:59'::timestamptz;
    -- Don't override modified_by if already set by application
    if new.modified_by is null or new.modified_by = '' then
        new.modified_by = current_user;
    end if;

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
