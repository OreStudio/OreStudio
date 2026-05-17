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
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * Tenant Table
 *
 * Core entity for multi-tenancy support. Each tenant represents an isolated
 * organisation with its own users, roles, and data. The system tenant (UUID all zeros)
 * is a special tenant used for shared reference data and system administration.
 *
 * Tenants are identified by:
 * - id: UUID primary key (SQL also has tenant_id = id for self-reference)
 * - code: Unique text code for stable referencing (e.g., 'system', 'acme')
 * - hostname: Unique hostname for tenant routing during login
 */

create table if not exists "ores_iam_tenants_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null default ores_utility_system_tenant_id_fn(),
    "version" integer not null,
    "code" text not null,
    "name" text not null,
    "type" text not null,
    "description" text null,
    "hostname" text not null,
    "status" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, valid_from, valid_to),
    exclude using gist (
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> 'ffffffff-ffff-ffff-ffff-ffffffffffff'::uuid or "code" = 'system'),
    check ("tenant_id" = ores_utility_system_tenant_id_fn()),
    check ("code" <> ''),
    check ("hostname" <> '')
);

-- Unique code for active records
create unique index if not exists tenants_code_uniq_idx
on "ores_iam_tenants_tbl" (code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists tenants_version_uniq_idx
on "ores_iam_tenants_tbl" (id, version)
where valid_to = ores_utility_infinity_timestamp_fn();


create unique index if not exists tenants_hostname_uniq_idx
on "ores_iam_tenants_tbl" (hostname)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_iam_tenants_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- All tenants belong to the system tenant
    NEW.tenant_id := ores_utility_system_tenant_id_fn();

    -- Validate type FK
    if not exists (
        select 1 from ores_iam_tenant_types_tbl
        where type = NEW.type
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid tenant type: %. Must exist in ores_iam_tenant_types_tbl.',
            NEW.type using errcode = '23503';
    end if;

    -- Validate status FK
    if not exists (
        select 1 from ores_iam_tenant_statuses_tbl
        where status = NEW.status
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid tenant status: %. Must exist in ores_iam_tenant_statuses_tbl.',
            NEW.status using errcode = '23503';
    end if;

    -- Validate change_reason_code (use system tenant for tenants records)
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(ores_utility_system_tenant_id_fn(), NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_iam_tenants_tbl"
    where id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update "ores_iam_tenants_tbl"
        set valid_to = current_timestamp
        where id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_iam_tenants_insert_trg
before insert on "ores_iam_tenants_tbl"
for each row execute function ores_iam_tenants_insert_fn();

create or replace rule ores_iam_tenants_delete_rule as
on delete to "ores_iam_tenants_tbl" do instead
    update "ores_iam_tenants_tbl"
    set valid_to = current_timestamp,
        status = 'terminated'
    where id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
