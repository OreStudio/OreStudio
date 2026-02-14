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
set schema 'public';

-- =============================================================================
-- Tenant Statuses - Valid tenant lifecycle statuses
-- =============================================================================

create table if not exists "ores_iam_tenant_statuses_tbl" (
    "status" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "display_order" integer not null default 0,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, status, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        status WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("status" <> ''),
    check ("change_reason_code" <> '')
);

create unique index if not exists ores_iam_tenant_statuses_version_uniq_idx
on "public"."ores_iam_tenant_statuses_tbl" (tenant_id, status, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_iam_tenant_statuses_status_uniq_idx
on "public"."ores_iam_tenant_statuses_tbl" (tenant_id, status)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_iam_tenant_statuses_tenant_idx
on "public"."ores_iam_tenant_statuses_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function public.ores_iam_tenant_statuses_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Validate change_reason_code
    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    select version into current_version
    from "public"."ores_iam_tenant_statuses_tbl"
    where tenant_id = new.tenant_id
      and status = new.status
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "public"."ores_iam_tenant_statuses_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
          and status = new.status
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();
    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = current_user;

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_iam_tenant_statuses_insert_trg
before insert on "public"."ores_iam_tenant_statuses_tbl"
for each row
execute function public.ores_iam_tenant_statuses_insert_fn();

create or replace rule ores_iam_tenant_statuses_delete_rule as
on delete to "public"."ores_iam_tenant_statuses_tbl"
do instead
  update "public"."ores_iam_tenant_statuses_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and status = old.status
  and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Validation function for tenant_status
-- Validates that a status exists in the tenant_statuses table.
-- Returns the validated value, or default if null/empty.
-- Uses system tenant data (shared reference data).
-- =============================================================================
create or replace function ores_iam_validate_tenant_status_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- Return default if null or empty
    if p_value is null or p_value = '' then
        raise exception 'Invalid tenant_status: value cannot be null or empty'
            using errcode = '23502';
    end if;

    -- Allow pass-through during bootstrap (empty table)
    if not exists (select 1 from ores_iam_tenant_statuses_tbl limit 1) then
        return p_value;
    end if;

    -- Validate against reference data
    if not exists (
        select 1 from ores_iam_tenant_statuses_tbl
        where tenant_id = ores_iam_system_tenant_id_fn()
          and status = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid tenant_status: %. Must be one of: %', p_value, (
            select string_agg(status::text, ', ' order by display_order)
            from ores_iam_tenant_statuses_tbl
            where tenant_id = ores_iam_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql;
