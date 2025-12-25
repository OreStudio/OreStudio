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
set schema 'ores';

--
-- image_tags table: Junction table for many-to-many relationship between images and tags
--
create table if not exists "ores"."image_tags" (
    "image_id" uuid not null,
    "tag_id" uuid not null,
    "assigned_by" text not null,
    "assigned_at" timestamp with time zone not null default current_timestamp,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (image_id, tag_id, valid_from),
    exclude using gist (
        image_id WITH =,
        tag_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for lookups by image
create index if not exists image_tags_image_idx
on "ores"."image_tags" (image_id)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

-- Index for lookups by tag
create index if not exists image_tags_tag_idx
on "ores"."image_tags" (tag_id)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

create or replace function update_image_tags()
returns trigger as $$
begin
    -- Close any existing current record for this image_id and tag_id pair
    update "ores"."image_tags"
    set valid_to = current_timestamp
    where image_id = new.image_id and tag_id = new.tag_id
    and valid_to = '9999-12-31 23:59:59'::timestamptz;

    new.valid_from = current_timestamp;
    new.valid_to = '9999-12-31 23:59:59'::timestamptz;
    new.assigned_at = current_timestamp;
    -- Don't override assigned_by if already set by application
    if new.assigned_by is null or new.assigned_by = '' then
        new.assigned_by = current_user;
    end if;

    return new;
end;
$$ language plpgsql;

create or replace trigger update_image_tags_trigger
before insert on "ores"."image_tags"
for each row
execute function update_image_tags();

-- Use a RULE for soft deletes
create or replace rule delete_image_tags_rule as
on delete to "ores"."image_tags"
do instead
  update "ores"."image_tags"
  set valid_to = current_timestamp
  where image_id = old.image_id
  and tag_id = old.tag_id
  and valid_to = '9999-12-31 23:59:59'::timestamptz;
