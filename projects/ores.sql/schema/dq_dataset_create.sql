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

create table if not exists "ores"."dq_dataset_tbl" (
    "id" uuid not null,
    "version" integer not null,
    "subject_area_id" uuid not null,
    "origin_id" uuid not null,
    "nature_id" uuid not null,
    "treatment_id" uuid not null,
    "methodology_id" uuid,
    "name" text not null,
    "description" text not null,
    "source_system_id" text not null,
    "business_context" text not null,
    "upstream_derivation_id" uuid,
    "lineage_depth" integer not null,
    "as_of_date" date not null,
    "ingestion_timestamp" timestamp with time zone not null,
    "license_info" text,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, valid_from, valid_to),
    foreign key (subject_area_id) references ores.dq_subject_area_tbl(id),
    foreign key (origin_id) references ores.dq_origin_dimension_tbl(id),
    foreign key (nature_id) references ores.dq_nature_dimension_tbl(id),
    foreign key (treatment_id) references ores.dq_treatment_dimension_tbl(id),
    foreign key (methodology_id) references ores.dq_methodology_tbl(id),
    foreign key (upstream_derivation_id) references ores.dq_dataset_tbl(id),
    exclude using gist (
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

create unique index if not exists dq_dataset_name_uniq_idx
on "ores"."dq_dataset_tbl" (name, subject_area_id)
where valid_to = ores.utility_infinity_timestamp_fn();

create or replace function ores.dq_dataset_insert_fn()
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

    if NEW.upstream_derivation_id is null then
        NEW.lineage_depth := 0;
    else
        select COALESCE((select lineage_depth from ores.dq_dataset_tbl 
                         where id = NEW.upstream_derivation_id 
                         and valid_to = ores.utility_infinity_timestamp_fn()), 0) + 1
        into NEW.lineage_depth;
    end if;

    NEW.change_reason_code := ores.refdata_validate_change_reason_fn(NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger dq_dataset_insert_trg
before insert on "ores"."dq_dataset_tbl"
for each row execute function ores.dq_dataset_insert_fn();

create or replace rule dq_dataset_delete_rule as
on delete to "ores"."dq_dataset_tbl" do instead
    update "ores"."dq_dataset_tbl"
    set valid_to = current_timestamp
    where id = OLD.id
      and valid_to = ores.utility_infinity_timestamp_fn();