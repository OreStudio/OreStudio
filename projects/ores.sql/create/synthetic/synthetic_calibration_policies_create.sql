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
 * Calibration Policy Table
 *
 * A policy is the class-wide answer to how a feed is calibrated: the volatility
 * window in days, whether a pair may be read the other way round as 1/x, and
 * what to do when no candidate has a usable fixing. The provider order lives in
 * calibration_provider_order rows.
 *
 * A policy row is optional. When no row exists for a class, the defaults apply:
 * providers in catalogue display order, a 90-day window, an inverse allowed and
 * the keep_fixed fallback. The system tenant owns the rows and every tenant
 * reads them.
 */

create table if not exists "ores_synthetic_calibration_policies_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "asset_class_code" text not null,
    "window_days" integer not null,
    "allow_inverse" boolean not null,
    "fallback_code" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
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
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("window_days" between 2 and 180)
);

-- Unique asset_class_code for active records
create unique index if not exists calibration_policies_asset_class_code_uniq_idx
on "ores_synthetic_calibration_policies_tbl" (tenant_id, asset_class_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists calibration_policies_version_uniq_idx
on "ores_synthetic_calibration_policies_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists calibration_policies_id_uniq_idx
on "ores_synthetic_calibration_policies_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists calibration_policies_tenant_idx
on "ores_synthetic_calibration_policies_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_synthetic_calibration_policies_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate asset_class_code
    NEW.asset_class_code := ores_refdata_validate_asset_class_code_fn(NEW.tenant_id, NEW.asset_class_code);

    -- Validate fallback_code
    NEW.fallback_code := ores_synthetic_validate_calibration_fallback_fn(NEW.tenant_id, NEW.fallback_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_synthetic_calibration_policies_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        -- The write states what it believes about the row, and the store is
        -- what decides. Version zero means one thing: no current row exists.
        -- So a create that collides with a live row is refused here, for every
        -- client, rather than by a check each client has to remember.
        if NEW.version = 0 then
            if not ores_utility_version_replace_allowed_fn() then
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'calibration_policy',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'calibration_policy',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_synthetic_calibration_policies_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_synthetic_calibration_policies_insert_trg
before insert on "ores_synthetic_calibration_policies_tbl"
for each row execute function ores_synthetic_calibration_policies_insert_fn();

create or replace rule ores_synthetic_calibration_policies_delete_rule as
on delete to "ores_synthetic_calibration_policies_tbl" do instead (
    update "ores_synthetic_calibration_policies_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      -- A tenant reads the shared rows of the system tenant, so OLD can be one.
      -- The rule runs with the owner's rights, which row-level security does
      -- not bind, so the rule states the own-tenant limit that the delete
      -- policy cannot enforce here.
      and tenant_id = ores_iam_current_tenant_id_fn()
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Validation function for calibration_policy
-- Validates that a id exists in the calibration_policies table.
-- Returns the validated value, or default if null/empty.
-- Uses system tenant data (shared reference data).
-- =============================================================================
create or replace function ores_synthetic_validate_calibration_policy_fn(
    p_tenant_id uuid,
    p_value uuid
) returns uuid as $$
begin
    -- Return default if null or empty
    if p_value is null then
        perform ores_outcome_raise_fn('missing_field', 'calibration_policy');
    end if;

    -- Allow pass-through during bootstrap (no active rows for system tenant).
    if not exists (
        select 1 from ores_synthetic_calibration_policies_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    -- Validate against reference data
    if not exists (
        select 1 from ores_synthetic_calibration_policies_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid calibration_policy: %. Must be one of: %', p_value, (
            select string_agg(id::text, ', ' order by asset_class_code)
            from ores_synthetic_calibration_policies_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Row-level security: tenant isolation for Calibration Policy
-- Every session reads its own rows and the system tenant's rows; writes
-- stay the current tenant's.
-- =============================================================================
alter table ores_synthetic_calibration_policies_tbl enable row level security;

drop policy if exists calibration_policies_tbl_tenant_isolation_policy
    on ores_synthetic_calibration_policies_tbl;

create policy calibration_policies_tbl_tenant_isolation_policy
on ores_synthetic_calibration_policies_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
