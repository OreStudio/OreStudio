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
 * Yield Curve Process Parameter Definition Table
 *
 * Reference data table describing, per yield_curve_process_type, the
 * named parameters that process accepts and their validation bounds. This
 * is the "vocabulary" half of the row-based parameter architecture: an
 * ir_curve_generation_config stores its process parameters as
 * ir_curve_generation_config_process_parameter_value rows, one per
 * definition, and the mapping layer materialises those rows into the
 * strongly-typed process-parameter structs of ores.analytics.quant
 * (two_factor_gaussian_params, vasicek_params, ...). The
 * (process_type_code, parameter_name) pair uniquely identifies a
 * parameter; four fields drive the Qt parameter table: display_name
 * (the English name), symbol (the Greek letter, where one is
 * conventional), short_label (the layperson name shown in Simple
 * mode) and description (the rich tooltip text). min_value/
 * max_value (NULL = unbounded) plus default_value drive the
 * dialog's spin-box ranges and pre-fill.
 *
 * Why this exists as a table rather than hardcoded structs: it makes the
 * parameter vocabulary queryable and extensible -- adding a new model or
 * parameter is a seed-data change, not a schema or code change, and the
 * same vocabulary drives the DB (validation of value rows), the mapping
 * layer (expected parameter names), and the UI (rows to display). Managed
 * by the system tenant as read-only reference data; tenant users never
 * edit definitions, only the values of their own configs.
 */

create table if not exists "ores_synthetic_process_parameter_definitions_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "process_type_code" text not null,
    "parameter_name" text not null,
    "display_name" text not null,
    "symbol" text null,
    "short_label" text not null,
    "description" text not null,
    "data_type" text not null,
    "default_value" double precision not null,
    "min_value" double precision null,
    "max_value" double precision null,
    "display_order" integer not null,
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
    check ("parameter_name" <> ''),
    check ("process_type_code" <> '')
);

-- Composite natural key: unique combination for active records
create unique index if not exists yield_curve_process_parameter_definitions_process_type_code_parameter_name_uniq_idx
on "ores_synthetic_process_parameter_definitions_tbl" (tenant_id, process_type_code, parameter_name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists yield_curve_process_parameter_definitions_version_uniq_idx
on "ores_synthetic_process_parameter_definitions_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists yield_curve_process_parameter_definitions_id_uniq_idx
on "ores_synthetic_process_parameter_definitions_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists yield_curve_process_parameter_definitions_tenant_idx
on "ores_synthetic_process_parameter_definitions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_synthetic_process_parameter_definitions_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate process_type_code
    NEW.process_type_code := ores_synthetic_validate_yield_curve_process_type_fn(NEW.tenant_id, NEW.process_type_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_synthetic_process_parameter_definitions_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
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
        update "ores_synthetic_process_parameter_definitions_tbl"
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
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_synthetic_process_parameter_definitions_insert_trg
before insert on "ores_synthetic_process_parameter_definitions_tbl"
for each row execute function ores_synthetic_process_parameter_definitions_insert_fn();

create or replace rule ores_synthetic_process_parameter_definitions_delete_rule as
on delete to "ores_synthetic_process_parameter_definitions_tbl" do instead (
    update "ores_synthetic_process_parameter_definitions_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Validation function for yield_curve_process_parameter_definition
-- Validates that a id exists in the yield_curve_process_parameter_definitions table.
-- Returns the validated value, or default if null/empty.
-- Uses system tenant data (shared reference data).
-- =============================================================================
create or replace function ores_synthetic_validate_yield_curve_process_parameter_definition_fn(
    p_tenant_id uuid,
    p_value uuid
) returns uuid as $$
begin
    -- Return default if null or empty
    if p_value is null then
        raise exception 'Invalid yield_curve_process_parameter_definition: value cannot be null'
            using errcode = '23502';
    end if;

    -- Allow pass-through during bootstrap (no active rows for system tenant).
    if not exists (
        select 1 from ores_synthetic_process_parameter_definitions_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    -- Validate against reference data
    if not exists (
        select 1 from ores_synthetic_process_parameter_definitions_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid yield_curve_process_parameter_definition: %. Must be one of: %', p_value, (
            select string_agg(id::text, ', ' order by display_order)
            from ores_synthetic_process_parameter_definitions_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
