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
set schema 'production';

-- =============================================================================
-- SVG image storage.
-- Key provides human-readable lookup.
-- =============================================================================

create table if not exists "production"."assets_images_tbl" (
    "image_id" uuid not null,
    "version" integer not null,
    "key" text not null,
    "description" text not null,
    "svg_data" text not null,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (image_id, valid_from, valid_to),
    exclude using gist (
        image_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("change_reason_code" <> '')
);

create unique index if not exists assets_images_version_uniq_idx
on "production"."assets_images_tbl" (image_id, version)
where valid_to = public.utility_infinity_timestamp_fn();

create unique index if not exists assets_images_key_uniq_idx
on "production"."assets_images_tbl" (key)
where valid_to = public.utility_infinity_timestamp_fn();

create or replace function production.assets_images_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    select version into current_version
    from "production"."assets_images_tbl"
    where image_id = new.image_id
    and valid_to = public.utility_infinity_timestamp_fn();

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "production"."assets_images_tbl"
        set valid_to = current_timestamp
        where image_id = new.image_id
        and valid_to = public.utility_infinity_timestamp_fn()
        and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = public.utility_infinity_timestamp_fn();
    if new.modified_by is null or new.modified_by = '' then
        new.modified_by = current_user;
    end if;

    new.change_reason_code := metadata.refdata_validate_change_reason_fn(new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger assets_images_insert_trg
before insert on "production"."assets_images_tbl"
for each row
execute function production.assets_images_insert_fn();

create or replace rule assets_images_delete_rule as
on delete to "production"."assets_images_tbl"
do instead
  update "production"."assets_images_tbl"
  set valid_to = current_timestamp
  where image_id = old.image_id
  and valid_to = public.utility_infinity_timestamp_fn();
