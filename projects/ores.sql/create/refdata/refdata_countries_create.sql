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

-- =============================================================================
-- ISO 3166-1 country definitions.
-- Includes alpha-2, alpha-3, and numeric codes.
-- Optional flag image reference.
-- coding_scheme_code tracks data provenance.
-- =============================================================================

create table if not exists "ores"."refdata_countries_tbl" (
    "alpha2_code" text not null,
    "version" integer not null,
    "alpha3_code" text not null,
    "numeric_code" text not null,
    "name" text not null,
    "official_name" text not null,
    "coding_scheme_code" text,
    "image_id" uuid,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (alpha2_code, valid_from, valid_to),
    exclude using gist (
        alpha2_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("alpha2_code" <> ''),
    check ("change_reason_code" <> '')
);

create unique index if not exists refdata_countries_version_uniq_idx
on "ores"."refdata_countries_tbl" (alpha2_code, version)
where valid_to = ores.utility_infinity_timestamp_fn();

create index if not exists refdata_countries_alpha3_idx
on "ores"."refdata_countries_tbl" (alpha3_code)
where valid_to = ores.utility_infinity_timestamp_fn();

create index if not exists refdata_countries_numeric_idx
on "ores"."refdata_countries_tbl" (numeric_code)
where valid_to = ores.utility_infinity_timestamp_fn();

create or replace function ores.refdata_countries_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate foreign key references
    if NEW.coding_scheme_code is not null and not exists (
        select 1 from ores.dq_coding_schemes_tbl
        where code = NEW.coding_scheme_code
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid coding_scheme_code: %. Coding scheme must exist.', NEW.coding_scheme_code
        using errcode = '23503';
    end if;

    select version into current_version
    from "ores"."refdata_countries_tbl"
    where alpha2_code = new.alpha2_code
    and valid_to = ores.utility_infinity_timestamp_fn();

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "ores"."refdata_countries_tbl"
        set valid_to = current_timestamp
        where alpha2_code = new.alpha2_code
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

    new.change_reason_code := ores.refdata_validate_change_reason_fn(new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger refdata_countries_insert_trg
before insert on "ores"."refdata_countries_tbl"
for each row
execute function ores.refdata_countries_insert_fn();

create or replace rule refdata_countries_delete_rule as
on delete to "ores"."refdata_countries_tbl"
do instead
  update "ores"."refdata_countries_tbl"
  set valid_to = current_timestamp
  where alpha2_code = old.alpha2_code
  and valid_to = ores.utility_infinity_timestamp_fn();
