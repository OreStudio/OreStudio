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
 * Data Quality Dataset Dependency Table
 *
 * Declares dependencies between datasets using stable dataset codes.
 * When a dataset is injected into the system, all its dependencies must
 * be satisfied first. Dependencies are resolved by code at injection time.
 *
 * Note: dependency_code is intentionally NOT a foreign key to allow:
 * - Declaring dependencies on datasets that may not exist yet
 * - Referencing datasets in external systems
 * - Loose coupling with validation at injection time
 *
 * Examples:
 * - "iso.countries" depends on "assets.country_flags" for visual display
 * - "crypto.large" and "crypto.small" depend on "assets.crypto_icons" for icon display
 */

create table if not exists "ores_dq_dataset_dependencies_tbl" (
    "dataset_code" text not null,
    "tenant_id" uuid not null,
    "dependency_code" text not null,
    "role" text not null,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, dataset_code, dependency_code, valid_from),
    exclude using gist (
        tenant_id WITH =,
        dataset_code WITH =,
        dependency_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("change_reason_code" <> ''),
    check ("dataset_code" <> "dependency_code")  -- Prevent self-dependency
);

-- Index for looking up dependencies of a dataset
create index if not exists ores_dq_dataset_dependencies_dataset_idx
on "ores_dq_dataset_dependencies_tbl" (dataset_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding datasets that depend on a given dataset
create index if not exists ores_dq_dataset_dependencies_dependency_idx
on "ores_dq_dataset_dependencies_tbl" (dependency_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists ores_dq_dataset_dependencies_uniq_idx
on "ores_dq_dataset_dependencies_tbl" (tenant_id, dataset_code, dependency_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_dq_dataset_dependencies_tenant_idx
on "ores_dq_dataset_dependencies_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_dq_dataset_dependencies_insert_fn()
returns trigger as $$
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Close any existing record for this dependency
    update "ores_dq_dataset_dependencies_tbl"
    set valid_to = current_timestamp
    where tenant_id = new.tenant_id
    and dataset_code = new.dataset_code
    and dependency_code = new.dependency_code
    and valid_to = ores_utility_infinity_timestamp_fn()
    and valid_from < current_timestamp;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();

    if new.modified_by is null or new.modified_by = '' then
        new.modified_by = current_user;
    end if;

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_dq_dataset_dependencies_insert_trg
before insert on "ores_dq_dataset_dependencies_tbl"
for each row
execute function ores_dq_dataset_dependencies_insert_fn();

create or replace rule ores_dq_dataset_dependencies_delete_rule as
on delete to "ores_dq_dataset_dependencies_tbl"
do instead
  update "ores_dq_dataset_dependencies_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and dataset_code = old.dataset_code
  and dependency_code = old.dependency_code
  and valid_to = ores_utility_infinity_timestamp_fn();
