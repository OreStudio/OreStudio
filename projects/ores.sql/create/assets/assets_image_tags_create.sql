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

-- =============================================================================
-- Many-to-many: images to tags.
-- =============================================================================

create table if not exists "ores_assets_image_tags_tbl" (
    "image_id" uuid not null,
    "tenant_id" uuid not null,
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

create index if not exists ores_assets_image_tags_image_idx
on "ores_assets_image_tags_tbl" (image_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_assets_image_tags_tag_idx
on "ores_assets_image_tags_tbl" (tag_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_assets_image_tags_tenant_idx
on "ores_assets_image_tags_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_assets_image_tags_insert_fn()
returns trigger as $$
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    update "ores_assets_image_tags_tbl"
    set valid_to = current_timestamp
    where image_id = new.image_id and tag_id = new.tag_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();
    new.assigned_at = current_timestamp;
    new.assigned_by := ores_iam_validate_account_username_fn(coalesce(nullif(new.assigned_by, ''), ores_iam_current_actor_fn(), current_user));

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_assets_image_tags_insert_trg
before insert on "ores_assets_image_tags_tbl"
for each row
execute function ores_assets_image_tags_insert_fn();

create or replace rule ores_assets_image_tags_delete_rule as
on delete to "ores_assets_image_tags_tbl"
do instead
  update "ores_assets_image_tags_tbl"
  set valid_to = current_timestamp
  where image_id = old.image_id
  and tag_id = old.tag_id
  and valid_to = ores_utility_infinity_timestamp_fn();
