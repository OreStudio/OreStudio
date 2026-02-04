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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_junction_create.mustache
 * To modify, update the template and regenerate.
 *
 * Dataset Bundle Member Table
 *
 * Junction table linking bundles to their constituent datasets. Uses codes
 * for loose coupling - allows declaring membership for datasets that may not
 * yet exist in the system.
 *
 * Examples:
 * - Bundle "slovaris" contains "slovaris.countries", "slovaris.currencies", etc.
 * - Bundle "base" contains "iso.countries", "iso.currencies", all FpML datasets
 */

create table if not exists "ores_dq_dataset_bundle_members_tbl" (
    "bundle_code" text not null,
    "tenant_id" uuid not null,
    "dataset_code" text not null,
    "version" integer not null,
    "display_order" integer not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, bundle_code, dataset_code, valid_from),
    exclude using gist (
        tenant_id WITH =,
        bundle_code WITH =,
        dataset_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("change_reason_code" <> '')
);

-- Index for looking up datasets in a bundle
create index if not exists ores_dq_dataset_bundle_members_bundle_idx
on "ores_dq_dataset_bundle_members_tbl" (bundle_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding bundles containing a dataset
create index if not exists ores_dq_dataset_bundle_members_dataset_idx
on "ores_dq_dataset_bundle_members_tbl" (dataset_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists ores_dq_dataset_bundle_members_uniq_idx
on "ores_dq_dataset_bundle_members_tbl" (tenant_id, bundle_code, dataset_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_dq_dataset_bundle_members_tenant_idx
on "ores_dq_dataset_bundle_members_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_dq_dataset_bundle_members_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Version management
    select version into current_version
    from "ores_dq_dataset_bundle_members_tbl"
    where tenant_id = new.tenant_id
    and bundle_code = new.bundle_code
    and dataset_code = new.dataset_code
    and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        -- Close existing record
        update "ores_dq_dataset_bundle_members_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
        and bundle_code = new.bundle_code
        and dataset_code = new.dataset_code
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();

    if new.modified_by is null or new.modified_by = '' then
        new.modified_by = current_user;
    end if;

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_dq_dataset_bundle_members_insert_trg
before insert on "ores_dq_dataset_bundle_members_tbl"
for each row
execute function ores_dq_dataset_bundle_members_insert_fn();

create or replace rule ores_dq_dataset_bundle_members_delete_rule as
on delete to "ores_dq_dataset_bundle_members_tbl"
do instead
  update "ores_dq_dataset_bundle_members_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and bundle_code = old.bundle_code
  and dataset_code = old.dataset_code
  and valid_to = ores_utility_infinity_timestamp_fn();
