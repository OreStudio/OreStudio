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

create table if not exists "ores"."dq_subject_area_tbl" (
    "id" uuid not null,
    "version" integer not null,
    "domain_id" uuid not null,
    "name" text not null,
    "description" text not null,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, valid_from, valid_to),
    exclude using gist (
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

create unique index if not exists dq_subject_area_name_uniq_idx
on "ores"."dq_subject_area_tbl" (name, domain_id)
where valid_to = ores.utility_infinity_timestamp_fn();

create or replace function ores.dq_subject_area_insert_fn()
returns trigger as $$
begin
    if NEW.valid_from is null then
        NEW.valid_from := current_timestamp;
    end if;

    if NEW.valid_to is null then
        NEW.valid_to := ores.utility_infinity_timestamp_fn();
    end if;

    if NEW.version is null then
        NEW.version := 0;
    end if;

    -- Validate foreign key reference
    if not exists (
        select 1 from ores.dq_data_domain_tbl
        where id = NEW.domain_id
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid domain_id: %. Domain must exist.', NEW.domain_id
        using errcode = '23503';
    end if;

    NEW.change_reason_code := ores.refdata_validate_change_reason_fn(NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger dq_subject_area_insert_trg
before insert on "ores"."dq_subject_area_tbl"
for each row execute function ores.dq_subject_area_insert_fn();

create or replace rule dq_subject_area_delete_rule as
on delete to "ores"."dq_subject_area_tbl" do instead
    update "ores"."dq_subject_area_tbl"
    set valid_to = current_timestamp
    where id = OLD.id
      and valid_to = ores.utility_infinity_timestamp_fn();