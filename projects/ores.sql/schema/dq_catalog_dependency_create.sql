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
 * Data Quality Catalog Dependency Table
 *
 * Declares dependencies between catalogs. When a catalog is injected into
 * the system, all its dependencies must be satisfied first. Dependencies
 * are resolved by name at injection time.
 *
 * Note: dependency_name is intentionally NOT a foreign key to allow:
 * - Declaring dependencies on catalogs that may not exist yet
 * - Referencing catalogs in external systems
 * - Loose coupling with validation at injection time
 *
 * Examples:
 * - "Trade Data" depends on "ISO Reference Data"
 * - "Risk Analytics" depends on "Trade Data"
 */

set schema 'ores';

create table if not exists "ores"."dq_catalog_dependencies_tbl" (
    "catalog_name" text not null,
    "dependency_name" text not null,
    "recorded_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (catalog_name, dependency_name, valid_from),
    exclude using gist (
        catalog_name WITH =,
        dependency_name WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("change_reason_code" <> ''),
    check ("catalog_name" <> "dependency_name")  -- Prevent self-dependency
);

-- Index for looking up dependencies of a catalog
create index if not exists dq_catalog_dependencies_catalog_idx
on "ores"."dq_catalog_dependencies_tbl" (catalog_name)
where valid_to = ores.utility_infinity_timestamp_fn();

-- Index for finding catalogs that depend on a given catalog
create index if not exists dq_catalog_dependencies_dependency_idx
on "ores"."dq_catalog_dependencies_tbl" (dependency_name)
where valid_to = ores.utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists dq_catalog_dependencies_uniq_idx
on "ores"."dq_catalog_dependencies_tbl" (catalog_name, dependency_name)
where valid_to = ores.utility_infinity_timestamp_fn();

create or replace function ores.dq_catalog_dependencies_insert_fn()
returns trigger as $$
begin
    -- Close any existing record for this dependency
    update "ores"."dq_catalog_dependencies_tbl"
    set valid_to = current_timestamp
    where catalog_name = new.catalog_name
    and dependency_name = new.dependency_name
    and valid_to = ores.utility_infinity_timestamp_fn()
    and valid_from < current_timestamp;

    new.valid_from = current_timestamp;
    new.valid_to = ores.utility_infinity_timestamp_fn();

    if new.recorded_by is null or new.recorded_by = '' then
        new.recorded_by = current_user;
    end if;

    new.change_reason_code := ores.refdata_validate_change_reason_fn(new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger dq_catalog_dependencies_insert_trg
before insert on "ores"."dq_catalog_dependencies_tbl"
for each row
execute function ores.dq_catalog_dependencies_insert_fn();

create or replace rule dq_catalog_dependencies_delete_rule as
on delete to "ores"."dq_catalog_dependencies_tbl"
do instead
  update "ores"."dq_catalog_dependencies_tbl"
  set valid_to = current_timestamp
  where catalog_name = old.catalog_name
  and dependency_name = old.dependency_name
  and valid_to = ores.utility_infinity_timestamp_fn();
