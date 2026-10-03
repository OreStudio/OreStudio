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
 * Cap Floor Volatility Table
 *
 * The settings of one CapFloorVolatility entry of a curveconfig.xml, one row
 * per curve_definition in the CapFloorVolatilities section: the cap and floor
 * surface's tenors and strikes, the index and discount curve it is stripped with,
 * and how it interpolates. Its Report is a row of curve_report_configuration,
 * its BootstrapConfig a row of curve_bootstrap_config and its
 * ParametricSmileConfiguration a row of curve_parametric_smile.
 *
 * A ProxyConfig builds the surface from another one; it is one per entry, so
 * its fields are the proxy_ columns, and has_proxy_config says whether the
 * entry wrote it. The schema allows both UseEffeciveVolatility, a misspelling
 * kept for old documents, and UseEffectiveVolatility; each has its own column so
 * either round trips.
 */

create table if not exists "ores_refdata_cap_floor_volatilities_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "curve_definition_id" uuid not null,
    "volatility_type" text null,
    "output_volatility_type" text null,
    "model_shift" double precision null,
    "output_shift" double precision null,
    "extrapolation" text null,
    "interpolation_method" text null,
    "include_atm" text null,
    "day_counter" text null,
    "calendar" text null,
    "business_day_convention" text null,
    "tenors" text null,
    "strikes" text null,
    "optional_quotes" text null,
    "ibor_index" text null,
    "index" text null,
    "rate_computation_period" text null,
    "on_cap_settlement_days" integer null,
    "discount_curve" text null,
    "atm_tenors" text null,
    "settlement_days" integer null,
    "interpolate_on" text null,
    "time_interpolation" text null,
    "strike_interpolation" text null,
    "input_type" text null,
    "quote_includes_index_name" text null,
    "flat_first_period" text null,
    "use_effecive_volatility" text null,
    "use_effective_volatility" text null,
    "has_proxy_config" boolean not null,
    "proxy_source_curve_id" text null,
    "proxy_source_index" text null,
    "proxy_source_rate_computation_period" text null,
    "proxy_target_index" text null,
    "proxy_target_rate_computation_period" text null,
    "proxy_target_on_cap_settlement_days" integer null,
    "proxy_scaling_factor" double precision null,
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
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists cap_floor_volatilities_version_uniq_idx
on "ores_refdata_cap_floor_volatilities_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists cap_floor_volatilities_id_uniq_idx
on "ores_refdata_cap_floor_volatilities_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists cap_floor_volatilities_tenant_idx
on "ores_refdata_cap_floor_volatilities_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_cap_floor_volatilities_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate curve_definition_id (soft FK to ores_refdata_curve_definitions_tbl)
    if not exists (
        select 1 from ores_refdata_curve_definitions_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.curve_definition_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid curve_definition_id: %. No active curve definition found with this id.', NEW.curve_definition_id
            using errcode = '23503';
    end if;

    -- Validate day_counter (optional soft FK to ores_refdata_day_counters_tbl)
    if NEW.day_counter is not null then
        if not exists (
            select 1 from ores_refdata_day_counters_tbl
            where tenant_id = NEW.tenant_id
              and code = NEW.day_counter
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid day_counter: %. No active day counter found with this spelling.', NEW.day_counter
                using errcode = '23503';
        end if;
    end if;

    -- Validate calendar (optional field -- skip validation when null)
    if NEW.calendar is not null then
        NEW.calendar := ores_refdata_validate_calendar_fn(NEW.tenant_id, NEW.calendar);
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_refdata_cap_floor_volatilities_tbl"
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
                raise exception
                    'Row already exists: a create cannot replace it. State the version you read to replace the row, or ask for a version replace.'
                    using errcode = '23505';
            end if;
        elsif NEW.version != current_version then
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
        update "ores_refdata_cap_floor_volatilities_tbl"
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

create or replace trigger ores_refdata_cap_floor_volatilities_insert_trg
before insert on "ores_refdata_cap_floor_volatilities_tbl"
for each row execute function ores_refdata_cap_floor_volatilities_insert_fn();

create or replace rule ores_refdata_cap_floor_volatilities_delete_rule as
on delete to "ores_refdata_cap_floor_volatilities_tbl" do instead (
    update "ores_refdata_cap_floor_volatilities_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
