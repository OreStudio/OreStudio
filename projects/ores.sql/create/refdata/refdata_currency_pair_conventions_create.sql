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
 * Currency Pair Convention Table
 *
 * Quoting and date-convention fields for a currency pair — pip factor,
 * tick size, calendars, business day convention, spot-relative/end-of-
 * month flags — folded in from the retired fx_convention entity (see
 * [[id:04A121FA-00D6-43EB-9B21-04EDC1FA493D][Currency pair support in reference data]]). Keyed 1:1 by
 * pair_code, the same value space as
 * [[id:E1EE950D-FC22-4DAB-A93F-8B2A15196031][ores.refdata.currency_pair]]'s own primary key — every pair has at
 * most one convention record, so a separate identifier scheme would be
 * pure overhead. Like every other soft-FK relationship in this codebase,
 * pair_code is validated via trigger, not a hard DB foreign key.
 */

create table if not exists "ores_refdata_currency_pair_conventions_tbl" (
    "pair_code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "pip_factor" double precision not null,
    "tick_size" double precision not null,
    "decimal_places" integer not null default 4,
    "business_day_convention" text null,
    "spot_relative" boolean null,
    "end_of_month" boolean null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, pair_code, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        pair_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("pair_code" <> '')
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists currency_pair_conventions_version_uniq_idx
on "ores_refdata_currency_pair_conventions_tbl" (tenant_id, pair_code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists currency_pair_conventions_pair_code_uniq_idx
on "ores_refdata_currency_pair_conventions_tbl" (tenant_id, pair_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists currency_pair_conventions_tenant_idx
on "ores_refdata_currency_pair_conventions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_currency_pair_conventions_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate pair_code
    NEW.pair_code := ores_refdata_validate_currency_pair_fn(NEW.tenant_id, NEW.pair_code);

    -- Validate business_day_convention (optional field -- skip validation when null)
    if NEW.business_day_convention is not null then
        NEW.business_day_convention := ores_refdata_validate_business_day_convention_type_fn(NEW.tenant_id, NEW.business_day_convention);
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_refdata_currency_pair_conventions_tbl"
    where tenant_id = NEW.tenant_id
      and pair_code = NEW.pair_code
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_currency_pair_conventions_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and pair_code = NEW.pair_code
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_refdata_currency_pair_conventions_insert_trg
before insert on "ores_refdata_currency_pair_conventions_tbl"
for each row execute function ores_refdata_currency_pair_conventions_insert_fn();

create or replace rule ores_refdata_currency_pair_conventions_delete_rule as
on delete to "ores_refdata_currency_pair_conventions_tbl" do instead (
    update "ores_refdata_currency_pair_conventions_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and pair_code = OLD.pair_code
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Validation function for currency_pair_convention
-- Validates that a pair_code exists in the currency_pair_conventions table.
-- Returns the validated value, or default if null/empty.
-- Validates against both the tenant's own data and the system tenant's canonical set.
-- =============================================================================
create or replace function ores_refdata_validate_currency_pair_convention_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- Return default if null or empty
    if p_value is null or p_value = '' then
        raise exception 'Invalid currency_pair_convention: value cannot be null or empty'
            using errcode = '23502';
    end if;

    -- Allow pass-through if neither this tenant nor the system tenant has
    -- seeded active currency_pair_conventions yet (freshly provisioned tenant).
    if not exists (
        select 1 from ores_refdata_currency_pair_conventions_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    -- Validate against this tenant's values and the system tenant's canonical set.
    if not exists (
        select 1 from ores_refdata_currency_pair_conventions_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and pair_code = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid currency_pair_convention: %. Must be one of: %', p_value, (
            select string_agg(pair_code::text, ', ' order by pair_code)
            from ores_refdata_currency_pair_conventions_tbl
            where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
