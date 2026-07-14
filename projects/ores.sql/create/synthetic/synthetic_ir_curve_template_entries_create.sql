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
 * IR Curve Template Entry Table
 *
 * One row of the raw instrument grid (the "Curve Template") an
 * ir_curve_generation_config publishes: which tenor, priced as which
 * instrument type (deposit-equivalent, FRA-equivalent, swap-equivalent --
 * see ores.refdata.instrument_code), in what order. Entries belong to a
 * parent config via ir_curve_config_id, the same
 * one-config-many-children shape fx_spot_generation_config's
 * gmm_component rows use. Every entry's published rate is derived from
 * the parent config's short-rate process's discount_factor() at the
 * tenor's maturity -- never an independently-noised value -- so the
 * published tick batch is, by construction, a slice of one internally
 * consistent curve. Party- and tenant-scoped.
 */

create table if not exists "ores_synthetic_ir_curve_template_entries_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "ir_curve_config_id" uuid not null,
    "sequence_index" integer not null,
    "tenor_code" text not null,
    "instrument_code" text not null,
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
    check ("tenor_code" <> ''),
    check ("instrument_code" <> '')
);

-- Composite natural key: unique combination for active records
create unique index if not exists ir_curve_template_entries_party_id_ir_curve_config_id_sequence_index_uniq_idx
on "ores_synthetic_ir_curve_template_entries_tbl" (tenant_id, party_id, ir_curve_config_id, sequence_index)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists ir_curve_template_entries_version_uniq_idx
on "ores_synthetic_ir_curve_template_entries_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ir_curve_template_entries_id_uniq_idx
on "ores_synthetic_ir_curve_template_entries_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ir_curve_template_entries_tenant_idx
on "ores_synthetic_ir_curve_template_entries_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_synthetic_ir_curve_template_entries_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate ir_curve_config_id (soft FK to ores_synthetic_ir_curve_generation_configs_tbl)
    if not exists (
        select 1 from ores_synthetic_ir_curve_generation_configs_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.ir_curve_config_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid ir_curve_config_id: %. No active ir_curve_generation_config found with this id.', NEW.ir_curve_config_id
            using errcode = '23503';
    end if;

    -- Validate instrument_code
    NEW.instrument_code := ores_refdata_validate_instrument_code_fn(NEW.tenant_id, NEW.instrument_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_synthetic_ir_curve_template_entries_tbl"
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
        update "ores_synthetic_ir_curve_template_entries_tbl"
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

create or replace trigger ores_synthetic_ir_curve_template_entries_insert_trg
before insert on "ores_synthetic_ir_curve_template_entries_tbl"
for each row execute function ores_synthetic_ir_curve_template_entries_insert_fn();

create or replace rule ores_synthetic_ir_curve_template_entries_delete_rule as
on delete to "ores_synthetic_ir_curve_template_entries_tbl" do instead (
    update "ores_synthetic_ir_curve_template_entries_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Validation function for ir_curve_template_entry
-- Validates that a id exists in the ir_curve_template_entries table.
-- Returns the validated value, or default if null/empty.
-- Validates against both the tenant's own data and the system tenant's canonical set.
-- =============================================================================
create or replace function ores_synthetic_validate_ir_curve_template_entry_fn(
    p_tenant_id uuid,
    p_value uuid
) returns uuid as $$
begin
    -- Return default if null or empty
    if p_value is null then
        raise exception 'Invalid ir_curve_template_entry: value cannot be null'
            using errcode = '23502';
    end if;

    -- Allow pass-through if neither this tenant nor the system tenant has
    -- seeded active ir_curve_template_entries yet (freshly provisioned tenant).
    if not exists (
        select 1 from ores_synthetic_ir_curve_template_entries_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    -- Validate against this tenant's values and the system tenant's canonical set.
    if not exists (
        select 1 from ores_synthetic_ir_curve_template_entries_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid ir_curve_template_entry: %. Must be one of: %', p_value, (
            select string_agg(id::text, ', ' order by id)
            from ores_synthetic_ir_curve_template_entries_tbl
            where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
