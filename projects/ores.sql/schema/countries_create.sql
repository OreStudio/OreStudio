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
-- ISO 3166-1 countries table with temporal support.
--
create table if not exists "ores"."countries" (
    "alpha2_code" text not null,
    "version" integer not null,
    "alpha3_code" text not null,
    "numeric_code" text not null,
    "name" text not null,
    "official_name" text not null,
    "image_id" uuid,  -- Optional reference to flag image in images table
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
    check ("change_reason_code" <> '')
);

-- Unique constraint on version for current records ensures version uniqueness per entity
create unique index if not exists countries_version_unique_idx
on "ores"."countries" (alpha2_code, version)
where valid_to = ores.infinity_timestamp();

-- Index for alpha3_code lookups
create index if not exists countries_alpha3_idx
on "ores"."countries" (alpha3_code)
where valid_to = ores.infinity_timestamp();

-- Index for numeric_code lookups
create index if not exists countries_numeric_idx
on "ores"."countries" (numeric_code)
where valid_to = ores.infinity_timestamp();

create or replace function update_countries()
returns trigger as $$
declare
    current_version integer;
begin
    -- Get the current version of the existing record (if any)
    select version into current_version
    from "ores"."countries"
    where alpha2_code = new.alpha2_code
    and valid_to = ores.infinity_timestamp();

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
        update "ores"."countries"
        set valid_to = current_timestamp
        where alpha2_code = new.alpha2_code
        and valid_to = ores.infinity_timestamp()
        and valid_from < current_timestamp;
    else
        -- New record: set initial version
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores.infinity_timestamp();
    -- Don't override modified_by if already set by application
    if new.modified_by is null or new.modified_by = '' then
        new.modified_by = current_user;
    end if;

    -- Validate and default change_reason_code
    new.change_reason_code := ores.validate_and_default_change_reason(new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger update_countries_trigger
before insert on "ores"."countries"
for each row
execute function update_countries();

-- Use a RULE instead of a trigger to avoid tuple modification conflicts
-- Rules rewrite the query before execution, so there's no conflict with the DELETE
create or replace rule delete_countries_rule as
on delete to "ores"."countries"
do instead
  update "ores"."countries"
  set valid_to = current_timestamp
  where alpha2_code = old.alpha2_code
  and valid_to = ores.infinity_timestamp();
