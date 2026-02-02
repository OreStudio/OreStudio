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
-- Tenants Table
-- Core table for multi-tenancy support. Each tenant represents an isolated
-- organisation or the system platform tenant.
--
-- Note: This table has both 'id' (standard UUID PK) and 'tenant_id' (self-reference).
-- For tenant records, tenant_id = id. This maintains consistency with the standard
-- pattern where all tables have tenant_id for isolation.
-- =============================================================================

create table if not exists "ores_iam_tenants_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "type" text not null,
    "code" text not null,
    "name" text not null,
    "description" text,
    "hostname" text not null,
    "status" text not null,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid or "code" = 'system'),
    check ("tenant_id" = "id"),  -- Self-referential constraint
    check ("change_reason_code" <> ''),
    check ("code" <> ''),
    check ("hostname" <> '')
);

-- Unique indexes for active records
create unique index if not exists ores_iam_tenants_code_uniq_idx
on ores_iam_tenants_tbl (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_iam_tenants_hostname_uniq_idx
on ores_iam_tenants_tbl (tenant_id, hostname)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_iam_tenants_version_uniq_idx
on ores_iam_tenants_tbl (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_iam_tenants_tenant_idx
on ores_iam_tenants_tbl (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Insert Trigger Function
-- Handles version management, temporal fields, and FK validation.
-- =============================================================================

create or replace function ores_iam_tenants_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Ensure tenant_id equals id (self-reference)
    new.tenant_id := new.id;

    -- Validate type FK
    if not exists (
        select 1 from ores_iam_tenant_types_tbl
        where type = new.type
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid tenant type: %. Must exist in ores_iam_tenant_types_tbl.',
            new.type using errcode = '23503';
    end if;

    -- Validate status FK
    if not exists (
        select 1 from ores_iam_tenant_statuses_tbl
        where status = new.status
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid tenant status: %. Must exist in ores_iam_tenant_statuses_tbl.',
            new.status using errcode = '23503';
    end if;

    -- Version management
    select version into current_version
    from ores_iam_tenants_tbl
    where tenant_id = new.tenant_id
      and id = new.id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update ores_iam_tenants_tbl
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
          and id = new.id
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

    -- Validate change_reason_code (use id as tenant_id since this is the tenant itself)
    new.change_reason_code := ores_dq_validate_change_reason_fn(new.id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_iam_tenants_insert_trg
before insert on ores_iam_tenants_tbl
for each row
execute function ores_iam_tenants_insert_fn();

-- =============================================================================
-- Delete Rule (Soft Delete)
-- =============================================================================

create or replace rule ores_iam_tenants_delete_rule as
on delete to ores_iam_tenants_tbl
do instead
    update ores_iam_tenants_tbl
    set valid_to = current_timestamp
    where tenant_id = old.tenant_id
      and id = old.id
      and valid_to = ores_utility_infinity_timestamp_fn();
