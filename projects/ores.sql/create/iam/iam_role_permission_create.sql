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
 * Role Permission Table
 *
 * Junction table granting permissions to IAM roles. A role holds several
 * permissions and a permission is held by several roles. A permission taken
 * away closes the row and deletes nothing, so the history of a role includes
 * the permissions it no longer holds.
 */

create table if not exists "ores_iam_role_permissions_tbl" (
    "role_id" uuid not null,
    "tenant_id" uuid not null,
    "permission_id" uuid not null,
    "version" integer not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, role_id, permission_id, valid_from),
    exclude using gist (
        tenant_id WITH =,
        role_id WITH =,
        permission_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for looking up the permissions of a role
create index if not exists role_permissions_role_idx
on "ores_iam_role_permissions_tbl" (role_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding roles that hold a permission
create index if not exists role_permissions_permission_idx
on "ores_iam_role_permissions_tbl" (permission_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists role_permissions_uniq_idx
on "ores_iam_role_permissions_tbl" (tenant_id, role_id, permission_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists role_permissions_tenant_idx
on "ores_iam_role_permissions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_iam_role_permissions_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- The actor is validated before the version management below: the
    -- validator accepts a username only while a current account row holds
    -- it, and a self write retires that row.
    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);

    -- Version management
    select version into current_version
    from "ores_iam_role_permissions_tbl"
    where tenant_id = new.tenant_id
    and role_id = new.role_id
    and permission_id = new.permission_id
    and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'role_permissions',
                'role_id',
                new.version::text,
                current_version::text);
        end if;
        new.version = current_version + 1;

        -- Close existing record.
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. the same junction pair touched
        -- twice in one transaction) would collide with itself.
        -- clock_timestamp() always advances.
        update "ores_iam_role_permissions_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = new.tenant_id
        and role_id = new.role_id
        and permission_id = new.permission_id
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < clock_timestamp();
    else
        new.version = 1;
    end if;

    new.valid_from = clock_timestamp();
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    -- Validate role_id (soft FK to ores_iam_roles_tbl)
    if not exists (
        select 1 from ores_iam_roles_tbl
        where tenant_id = new.tenant_id
          and id = new.role_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid role_id: %. No active role found with this id.', new.role_id
            using errcode = '23503';
    end if;

    -- Validate permission_id (soft FK to ores_iam_permissions_tbl)
    if not exists (
        select 1 from ores_iam_permissions_tbl
        where tenant_id = new.tenant_id
          and id = new.permission_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid permission_id: %. No active permission found with this id.', new.permission_id
            using errcode = '23503';
    end if;

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_iam_role_permissions_insert_trg
before insert on "ores_iam_role_permissions_tbl"
for each row
execute function ores_iam_role_permissions_insert_fn();

create or replace rule ores_iam_role_permissions_delete_rule as
on delete to "ores_iam_role_permissions_tbl"
do instead
  update "ores_iam_role_permissions_tbl"
  set valid_to = clock_timestamp()
  where tenant_id = old.tenant_id
  and role_id = old.role_id
  and permission_id = old.permission_id
  and valid_to = ores_utility_infinity_timestamp_fn();
