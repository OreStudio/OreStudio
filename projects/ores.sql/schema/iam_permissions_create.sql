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

create table if not exists "ores"."iam_permissions_tbl" (
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

create unique index if not exists iam_permissions_code_uniq_idx
on "ores"."iam_permissions_tbl" (code)
where valid_to = ores.utility_infinity_timestamp_fn();

create or replace function ores.iam_permissions_insert_fn()
returns trigger as $$
begin
    update "ores"."iam_permissions_tbl"
    set valid_to = current_timestamp
    where id = new.id
    and valid_to = ores.utility_infinity_timestamp_fn()
    and valid_from < current_timestamp;

    new.valid_from = current_timestamp;
    new.valid_to = ores.utility_infinity_timestamp_fn();

    return new;
end;
$$ language plpgsql;

create or replace trigger iam_permissions_insert_trg
before insert on "ores"."iam_permissions_tbl"
for each row
execute function ores.iam_permissions_insert_fn();

create or replace rule iam_permissions_delete_rule as
on delete to "ores"."iam_permissions_tbl"
do instead
  update "ores"."iam_permissions_tbl"
  set valid_to = current_timestamp
  where id = old.id
  and valid_to = ores.utility_infinity_timestamp_fn();
