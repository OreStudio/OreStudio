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

/**
 * Data Quality Catalog Table
 *
 * A catalog is a named collection of related datasets. It provides a
 * high-level grouping mechanism for datasets that share a common theme,
 * source, or purpose.
 *
 * Examples:
 * - "ISO Standards" - Contains ISO 3166 countries, ISO 4217 currencies
 * - "Cryptocurrency" - Contains crypto reference data and icons
 */

create table if not exists "ores"."dq_catalogs_tbl" (
    "name" text not null,
    "version" integer not null,
    "description" text not null,
    "owner" text,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (name, valid_from, valid_to),
    exclude using gist (
        name WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

create unique index if not exists dq_catalogs_version_uniq_idx
on "ores"."dq_catalogs_tbl" (name, version)
where valid_to = ores.utility_infinity_timestamp_fn();

create unique index if not exists dq_catalogs_name_uniq_idx
on "ores"."dq_catalogs_tbl" (name)
where valid_to = ores.utility_infinity_timestamp_fn();

create or replace function ores.dq_catalogs_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    select version into current_version
    from "ores"."dq_catalogs_tbl"
    where name = NEW.name
      and valid_to = ores.utility_infinity_timestamp_fn();

    if found then
        -- This insert is an update. Check version and increment.
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        -- Close the old record.
        update "ores"."dq_catalogs_tbl"
        set valid_to = current_timestamp
        where name = NEW.name
          and valid_to = ores.utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        -- This is a new record.
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores.utility_infinity_timestamp_fn();

    if NEW.modified_by is null or NEW.modified_by = '' then
        NEW.modified_by = current_user;
    end if;

    NEW.change_reason_code := ores.refdata_validate_change_reason_fn(NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger dq_catalogs_insert_trg
before insert on "ores"."dq_catalogs_tbl"
for each row execute function ores.dq_catalogs_insert_fn();

create or replace rule dq_catalogs_delete_rule as
on delete to "ores"."dq_catalogs_tbl" do instead
    update "ores"."dq_catalogs_tbl"
    set valid_to = current_timestamp
    where name = OLD.name
      and valid_to = ores.utility_infinity_timestamp_fn();
