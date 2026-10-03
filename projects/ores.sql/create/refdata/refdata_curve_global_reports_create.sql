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
 * Curve Global Report Table
 *
 * The document level ReportConfiguration of a curveconfig.xml gives report
 * settings per curve family: FX, equity, commodity, swaption, cap and floor and
 * inflation cap and floor volatilities, yield curves and default curves. Each
 * family element the document writes is one row here, keyed to the document, and
 * has_report says whether the element holds a Report. An entry's own Report
 * overrides these and is a row of curve_report_configuration.
 *
 * The columns are those of curve_report_configuration. The yield curve family's
 * report writes only PillarDates.
 *
 * A ReportConfiguration element that writes no family appears in no corpus
 * document, and the mapper refuses it, because rows cannot record it.
 */

create table if not exists "ores_refdata_curve_global_reports_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "curve_configuration_id" uuid not null,
    "family" text not null,
    "has_report" boolean not null,
    "report_on_delta_grid" text null,
    "report_on_moneyness_grid" text null,
    "report_on_strike_grid" text null,
    "report_on_strike_spread_grid" text null,
    "deltas" text null,
    "moneyness" text null,
    "strikes" text null,
    "strike_spreads" text null,
    "expiries" text null,
    "pillar_dates" text null,
    "underlying_tenors" text null,
    "continuation_expiry" text null,
    "position" integer not null,
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
    check ("family" in ('FXVolatilities', 'EquityVolatilities', 'CommodityVolatilities', 'IRSwaptionVolatilities', 'IRCapFloorVolatilities', 'YieldCurves', 'InflationCapFloorVolatilities', 'DefaultCurves')),
    check ("position" >= 0)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists curve_global_reports_version_uniq_idx
on "ores_refdata_curve_global_reports_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists curve_global_reports_id_uniq_idx
on "ores_refdata_curve_global_reports_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists curve_global_reports_tenant_idx
on "ores_refdata_curve_global_reports_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_curve_global_reports_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate curve_configuration_id (soft FK to ores_refdata_curve_configurations_tbl)
    if not exists (
        select 1 from ores_refdata_curve_configurations_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.curve_configuration_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid curve_configuration_id: %. No active curve configuration found with this id.', NEW.curve_configuration_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_refdata_curve_global_reports_tbl"
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
        update "ores_refdata_curve_global_reports_tbl"
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

create or replace trigger ores_refdata_curve_global_reports_insert_trg
before insert on "ores_refdata_curve_global_reports_tbl"
for each row execute function ores_refdata_curve_global_reports_insert_fn();

create or replace rule ores_refdata_curve_global_reports_delete_rule as
on delete to "ores_refdata_curve_global_reports_tbl" do instead (
    update "ores_refdata_curve_global_reports_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
