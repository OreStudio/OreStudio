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
 * Amendment Reasons Table
 *
 * Defines the reasons that can be selected when amending or deleting records.
 * Each reason belongs to a category and specifies which operations it applies to.
 *
 * Reason codes use namespaced format: "category.reason_name"
 * Examples:
 * - system.new: Automatic reason for new record creation
 * - static_data.front_office_error: Correction of front office error
 * - static_data.operations_error: Correction of operations error
 * - static_data.touch: Touch/revalidate record without changes
 * - static_data.duplicate: Delete duplicate record
 * - static_data.other: Other reason (requires commentary)
 */

set schema 'ores';

create table if not exists "ores"."amendment_reasons" (
    "code" text not null,
    "version" integer not null,
    "description" text not null,
    "category_code" text not null,
    "applies_to_amend" boolean not null default true,
    "applies_to_delete" boolean not null default true,
    "requires_commentary" boolean not null default false,
    "display_order" integer not null default 0,
    "modified_by" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (code, valid_from, valid_to),
    exclude using gist (
        code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Unique constraint on version for current records ensures version uniqueness per entity
create unique index if not exists amendment_reasons_version_unique_idx
on "ores"."amendment_reasons" (code, version)
where valid_to = ores.infinity_timestamp();

-- Unique constraint on code for current records to support FK references
create unique index if not exists amendment_reasons_code_current_idx
on "ores"."amendment_reasons" (code)
where valid_to = ores.infinity_timestamp();

-- Index for looking up reasons by category
create index if not exists amendment_reasons_category_idx
on "ores"."amendment_reasons" (category_code)
where valid_to = ores.infinity_timestamp();

create or replace function update_amendment_reasons()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate that category_code exists in reason_categories
    if not exists (
        select 1 from "ores"."reason_categories"
        where code = new.category_code
        and valid_to = ores.infinity_timestamp()
    ) then
        raise exception 'Invalid category_code: %. Category must exist in reason_categories.',
            new.category_code
            using errcode = '23503';  -- foreign_key_violation
    end if;

    -- Get the current version of the existing record (if any)
    select version into current_version
    from "ores"."amendment_reasons"
    where code = new.code
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
        update "ores"."amendment_reasons"
        set valid_to = current_timestamp
        where code = new.code
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

    return new;
end;
$$ language plpgsql;

create or replace trigger update_amendment_reasons_trigger
before insert on "ores"."amendment_reasons"
for each row
execute function update_amendment_reasons();

-- Use a RULE instead of a trigger to avoid tuple modification conflicts
-- Rules rewrite the query before execution, so there's no conflict with the DELETE
create or replace rule delete_amendment_reasons_rule as
on delete to "ores"."amendment_reasons"
do instead
  update "ores"."amendment_reasons"
  set valid_to = current_timestamp
  where code = old.code
  and valid_to = ores.infinity_timestamp();
