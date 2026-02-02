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
-- ISO 4217 currency definitions
-- =============================================================================

create table if not exists "ores_refdata_currencies_tbl" (
    "iso_code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "numeric_code" text not null,
    "symbol" text not null,
    "fraction_symbol" text not null,
    "fractions_per_unit" integer not null,
    "rounding_type" text not null,
    "rounding_precision" integer not null,
    "format" text not null,
    "currency_type" text not null,
    "coding_scheme_code" text,
    "image_id" uuid,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, iso_code, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        iso_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("iso_code" <> ''),
    check ("change_reason_code" <> '')
);

create unique index if not exists ores_refdata_currencies_version_uniq_idx
on "public"."ores_refdata_currencies_tbl" (tenant_id, iso_code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_refdata_currencies_iso_code_uniq_idx
on "public"."ores_refdata_currencies_tbl" (tenant_id, iso_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_refdata_currencies_tenant_idx
on "public"."ores_refdata_currencies_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function public.ores_refdata_currencies_insert_fn()
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

    -- Validate rounding_type
    new.rounding_type := ores_refdata_validate_rounding_type_fn(new.tenant_id, new.rounding_type);

    select version into current_version
    from "public"."ores_refdata_currencies_tbl"
    where tenant_id = new.tenant_id
      and iso_code = new.iso_code
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "public"."ores_refdata_currencies_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
          and iso_code = new.iso_code
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

create or replace trigger ores_refdata_currencies_insert_trg
before insert on "public"."ores_refdata_currencies_tbl"
for each row
execute function public.ores_refdata_currencies_insert_fn();

create or replace rule ores_refdata_currencies_delete_rule as
on delete to "public"."ores_refdata_currencies_tbl"
do instead
  update "public"."ores_refdata_currencies_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and iso_code = old.iso_code
  and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Validation function for currency
-- Validates that a iso_code exists in the currencies table.
-- Returns the validated value, or default if null/empty.
-- Uses system tenant data (shared reference data).
-- =============================================================================
create or replace function ores_refdata_validate_currency_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- Return default if null or empty
    if p_value is null or p_value = '' then
        raise exception 'Invalid currency: value cannot be null or empty'
            using errcode = '23502';
    end if;

    -- Allow pass-through during bootstrap (empty table)
    if not exists (select 1 from ores_refdata_currencies_tbl limit 1) then
        return p_value;
    end if;

    -- Validate against reference data
    if not exists (
        select 1 from ores_refdata_currencies_tbl
        where tenant_id = ores_iam_system_tenant_id_fn()
          and iso_code = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid currency: %. Must be one of: %', p_value, (
            select string_agg(iso_code::text, ', ' order by iso_code)
            from ores_refdata_currencies_tbl
            where tenant_id = ores_iam_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql;
