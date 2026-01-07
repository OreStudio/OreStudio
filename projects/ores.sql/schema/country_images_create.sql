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
-- country_images table: Associates countries with their primary image (flag)
-- Each country has one primary image only
--
create table if not exists "ores"."country_images" (
    "alpha2_code" text not null,
    "image_id" uuid not null,
    "assigned_by" text not null,
    "assigned_at" timestamp with time zone not null default current_timestamp,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (alpha2_code, valid_from, valid_to),
    exclude using gist (
        alpha2_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for lookups by image (reverse lookup)
create index if not exists country_images_image_idx
on "ores"."country_images" (image_id)
where valid_to = '9999-12-31 23:59:59'::timestamptz;

create or replace function update_country_images()
returns trigger as $$
begin
    -- Close any existing current record for this alpha2_code
    update "ores"."country_images"
    set valid_to = current_timestamp
    where alpha2_code = new.alpha2_code
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

create or replace trigger update_country_images_trigger
before insert on "ores"."country_images"
for each row
execute function update_country_images();

-- Use a RULE for soft deletes
create or replace rule delete_country_images_rule as
on delete to "ores"."country_images"
do instead
  update "ores"."country_images"
  set valid_to = current_timestamp
  where alpha2_code = old.alpha2_code
  and valid_to = '9999-12-31 23:59:59'::timestamptz;
