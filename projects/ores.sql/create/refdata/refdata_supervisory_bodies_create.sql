/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
-- Contains a code representing a supervisory-body that may be supervising this transaction.
-- =============================================================================

create table if not exists "production"."refdata_supervisory_bodies_tbl" (
    "code" text not null,
    "version" integer not null,
    "source" text null,
    "description" text null,
    "coding_scheme_code" text not null,
    "modified_by" text not null,
    "change_reason_code" text not null,
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
    check ("change_reason_code" <> '')
);

create unique index if not exists refdata_supervisory_bodies_version_uniq_idx
on "production"."refdata_supervisory_bodies_tbl" (code, version)
where valid_to = public.utility_infinity_timestamp_fn();

create index if not exists refdata_supervisory_bodies_coding_scheme_idx
on "production"."refdata_supervisory_bodies_tbl" (coding_scheme_code)
where valid_to = public.utility_infinity_timestamp_fn();

create or replace function production.refdata_supervisory_bodies_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate foreign key references
    if NEW.coding_scheme_code is not null and not exists (
        select 1 from metadata.dq_coding_schemes_tbl
        where code = NEW.coding_scheme_code
        and valid_to = public.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid coding_scheme_code: %. Coding scheme must exist.', NEW.coding_scheme_code
        using errcode = '23503';
    end if;

    select version into current_version
    from "production"."refdata_supervisory_bodies_tbl"
    where code = new.code
      and coding_scheme_code = new.coding_scheme_code
    and valid_to = public.utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "production"."refdata_supervisory_bodies_tbl"
        set valid_to = current_timestamp
        where code = new.code
          and coding_scheme_code = new.coding_scheme_code
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

create or replace trigger refdata_supervisory_bodies_insert_trg
before insert on "production"."refdata_supervisory_bodies_tbl"
for each row
execute function production.refdata_supervisory_bodies_insert_fn();

create or replace rule refdata_supervisory_bodies_delete_rule as
on delete to "production"."refdata_supervisory_bodies_tbl"
do instead
  update "production"."refdata_supervisory_bodies_tbl"
  set valid_to = current_timestamp
  where code = old.code
  and coding_scheme_code = old.coding_scheme_code
  and valid_to = public.utility_infinity_timestamp_fn();
