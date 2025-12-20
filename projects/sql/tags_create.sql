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
-- tags table: Stores categories for images (e.g., 'flag', 'currency', 'commodity')
--
create table if not exists "oresdb"."tags" (
    "tag_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "modified_by" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tag_id, valid_from, valid_to),
    exclude using gist (
        tag_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Unique constraint on version for current records
create unique index if not exists tags_version_unique_idx
on "oresdb"."tags" (tag_id, version)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

-- Unique constraint on name for current records
create unique index if not exists tags_name_unique_idx
on "oresdb"."tags" (name)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

create or replace function update_tags()
returns trigger as $$
declare
    current_version integer;
begin
    -- Get the current version of the existing record (if any)
    select version into current_version
    from "oresdb"."tags"
    where tag_id = new.tag_id
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
        update "oresdb"."tags"
        set valid_to = current_timestamp
        where tag_id = new.tag_id
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

create or replace trigger update_tags_trigger
before insert on "oresdb"."tags"
for each row
execute function update_tags();

-- Use a RULE for soft deletes
create or replace rule delete_tags_rule as
on delete to "oresdb"."tags"
do instead
  update "oresdb"."tags"
  set valid_to = current_timestamp
  where tag_id = old.tag_id
  and valid_to = '9999-12-31 23:59:59'::timestamptz;
