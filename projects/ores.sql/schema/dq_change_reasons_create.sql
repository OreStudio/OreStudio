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

create table if not exists "ores"."dq_change_reasons_tbl" (
    "code" text not null,
    "version" integer not null,
    "description" text not null,
    "category_code" text not null,
    "applies_to_amend" boolean not null default true,
    "applies_to_delete" boolean not null default true,
    "requires_commentary" boolean not null default false,
    "display_order" integer not null default 0,
    "modified_by" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (code, valid_from, valid_to),
    exclude using gist (
        code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("code" <> ''),
    check ("category_code" <> '')
);

create unique index if not exists dq_change_reasons_version_uniq_idx
on "ores"."dq_change_reasons_tbl" (code, version)
where valid_to = ores.utility_infinity_timestamp_fn();

create unique index if not exists dq_change_reasons_code_uniq_idx
on "ores"."dq_change_reasons_tbl" (code)
where valid_to = ores.utility_infinity_timestamp_fn();

create index if not exists dq_change_reasons_category_idx
on "ores"."dq_change_reasons_tbl" (category_code)
where valid_to = ores.utility_infinity_timestamp_fn();

create or replace function ores.dq_change_reasons_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    if not exists (
        select 1 from "ores"."dq_change_reason_categories_tbl"
        where code = new.category_code
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid category_code: %. Category must exist in dq_change_reason_categories_tbl.',
            new.category_code
            using errcode = '23503';
    end if;

    select version into current_version
    from "ores"."dq_change_reasons_tbl"
    where code = new.code
    and valid_to = ores.utility_infinity_timestamp_fn();

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "ores"."dq_change_reasons_tbl"
        set valid_to = current_timestamp
        where code = new.code
        and valid_to = ores.utility_infinity_timestamp_fn()
        and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores.utility_infinity_timestamp_fn();
    if new.modified_by is null or new.modified_by = '' then
        new.modified_by = current_user;
    end if;

    return new;
end;
$$ language plpgsql;

create or replace trigger dq_change_reasons_insert_trg
before insert on "ores"."dq_change_reasons_tbl"
for each row
execute function ores.dq_change_reasons_insert_fn();

create or replace rule dq_change_reasons_delete_rule as
on delete to "ores"."dq_change_reasons_tbl"
do instead
  update "ores"."dq_change_reasons_tbl"
  set valid_to = current_timestamp
  where code = old.code
  and valid_to = ores.utility_infinity_timestamp_fn();
