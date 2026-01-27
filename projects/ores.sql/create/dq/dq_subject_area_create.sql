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
-- Sub-classification within a data domain.
-- Examples: currencies, countries, images.
-- =============================================================================

create table if not exists "metadata"."dq_subject_areas_tbl" (
    "name" text not null,
    "version" integer not null,
    "domain_name" text not null,
    "description" text not null,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (name, domain_name, valid_from, valid_to),
    exclude using gist (
        name WITH =,
        domain_name WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("name" <> ''),
    check ("domain_name" <> '')
);

create unique index if not exists dq_subject_areas_version_uniq_idx
on "metadata"."dq_subject_areas_tbl" (name, domain_name, version)
where valid_to = public.utility_infinity_timestamp_fn();

create unique index if not exists dq_subject_areas_name_uniq_idx
on "metadata"."dq_subject_areas_tbl" (name, domain_name)
where valid_to = public.utility_infinity_timestamp_fn();

create or replace function metadata.dq_subject_areas_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate foreign key reference
    if not exists (
        select 1 from metadata.dq_data_domains_tbl
        where name = NEW.domain_name
        and valid_to = public.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid domain_name: %. Domain must exist.', NEW.domain_name
        using errcode = '23503';
    end if;

    select version into current_version
    from "metadata"."dq_subject_areas_tbl"
    where name = NEW.name
      and domain_name = NEW.domain_name
      and valid_to = public.utility_infinity_timestamp_fn();

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update "metadata"."dq_subject_areas_tbl"
        set valid_to = current_timestamp
        where name = NEW.name
          and domain_name = NEW.domain_name
          and valid_to = public.utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = public.utility_infinity_timestamp_fn();

    if NEW.modified_by is null or NEW.modified_by = '' then
        NEW.modified_by = current_user;
    end if;

    NEW.change_reason_code := metadata.refdata_validate_change_reason_fn(NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger dq_subject_areas_insert_trg
before insert on "metadata"."dq_subject_areas_tbl"
for each row execute function metadata.dq_subject_areas_insert_fn();

create or replace rule dq_subject_areas_delete_rule as
on delete to "metadata"."dq_subject_areas_tbl" do instead
    update "metadata"."dq_subject_areas_tbl"
    set valid_to = current_timestamp
    where name = OLD.name
      and domain_name = OLD.domain_name
      and valid_to = public.utility_infinity_timestamp_fn();