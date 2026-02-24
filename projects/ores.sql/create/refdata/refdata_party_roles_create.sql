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
/*
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_table_create.mustache
 * To modify, update the template and regenerate.
 */
-- =============================================================================
-- Contains a code representing a related party role. This can be extended to provide custom roles.
-- =============================================================================

create table if not exists "ores_refdata_party_roles_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "source" text null,
    "description" text null,
    "coding_scheme_code" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, code, coding_scheme_code, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        code WITH =,
        coding_scheme_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("code" <> '')
);

create unique index if not exists ores_refdata_party_roles_version_uniq_idx
on "ores_refdata_party_roles_tbl" (tenant_id, code, coding_scheme_code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_refdata_party_roles_code_uniq_idx
on "ores_refdata_party_roles_tbl" (tenant_id, code, coding_scheme_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_refdata_party_roles_tenant_idx
on "ores_refdata_party_roles_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_refdata_party_roles_coding_scheme_idx
on "ores_refdata_party_roles_tbl" (coding_scheme_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_party_roles_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Validate foreign key references
    if NEW.coding_scheme_code is not null and not exists (
        select 1 from ores_dq_coding_schemes_tbl
        where code = NEW.coding_scheme_code
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid coding_scheme_code: %. Coding scheme must exist.', NEW.coding_scheme_code
        using errcode = '23503';
    end if;

    -- Validate change_reason_code
    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    select version into current_version
    from "ores_refdata_party_roles_tbl"
    where tenant_id = new.tenant_id
      and code = new.code
      and coding_scheme_code = new.coding_scheme_code
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "ores_refdata_party_roles_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
          and code = new.code
          and coding_scheme_code = new.coding_scheme_code
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();
    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_actor_fn(), current_user);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_refdata_party_roles_insert_trg
before insert on "ores_refdata_party_roles_tbl"
for each row
execute function ores_refdata_party_roles_insert_fn();

create or replace rule ores_refdata_party_roles_delete_rule as
on delete to "ores_refdata_party_roles_tbl"
do instead
  update "ores_refdata_party_roles_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and code = old.code
  and coding_scheme_code = old.coding_scheme_code
  and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Validation function for party_role
-- Validates that a code exists in the party_roles table.
-- Returns the validated value, or default if null/empty.
-- Uses current tenant data.
-- =============================================================================
create or replace function ores_refdata_validate_party_role_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- Return default if null or empty
    if p_value is null or p_value = '' then
        raise exception 'Invalid party_role: value cannot be null or empty'
            using errcode = '23502';
    end if;

    -- Allow pass-through during bootstrap (empty table)
    if not exists (select 1 from ores_refdata_party_roles_tbl limit 1) then
        return p_value;
    end if;

    -- Validate against reference data
    if not exists (
        select 1 from ores_refdata_party_roles_tbl
        where tenant_id = p_tenant_id
          and code = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_role: %. Must be one of: %', p_value, (
            select string_agg(code::text, ', ' order by code)
            from ores_refdata_party_roles_tbl
            where tenant_id = p_tenant_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql;
