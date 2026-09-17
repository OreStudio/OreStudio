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
 * Risk Report Config Table
 *
 * The ORE-level parameters for a risk report definition: base currency,
 * observation model, analytics flags, XVA/VaR/SIMM settings and threading.
 *
 * Each row is owned by exactly one report_definition (1:1, enforced by the
 * unique index on report_definition_id). Portfolio and book scope live in
 * separate temporal junction tables; an empty set in either junction means
 * "all visible to the tenant".
 *
 * Analytics flags use integer 0/1, not boolean, to match the project
 * convention. npv and cashflow default to enabled; all others default off.
 */

create table if not exists "ores_reporting_risk_report_configs_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "report_definition_id" uuid not null,
    "base_currency" text not null,
    "observation_model" text not null default 'disable',
    "n_threads" integer not null default 1,
    "market_data_type" text not null default 'eod',
    "market_data_date" date null,
    "npv_enabled" integer not null default 1,
    "cashflow_enabled" integer not null default 1,
    "curves_enabled" integer not null default 0,
    "sensitivity_enabled" integer not null default 0,
    "simulation_enabled" integer not null default 0,
    "xva_enabled" integer not null default 0,
    "stress_enabled" integer not null default 0,
    "parametric_var_enabled" integer not null default 0,
    "initial_margin_enabled" integer not null default 0,
    "pfe_enabled" integer not null default 0,
    "xva_quantile" numeric(5,4) null,
    "xva_cva_enabled" integer not null default 1,
    "xva_dva_enabled" integer not null default 0,
    "xva_fva_enabled" integer not null default 0,
    "xva_colva_enabled" integer not null default 0,
    "xva_dim_enabled" integer not null default 0,
    "xva_dim_quantile" numeric(5,4) null,
    "xva_dim_horizon_calendar_days" integer null,
    "xva_dim_regression_order" integer null,
    "var_quantiles" numeric(5,4)[] null,
    "var_method" text null,
    "simm_version" text null,
    "simm_calculation_currency" text null,
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
    check ("base_currency" <> ''),
    check ("observation_model" in ('disable', 'none', 'move', 'defer')),
    check ("n_threads" >= 1),
    check ("market_data_type" in ('live', 'eod', 'date')),
    check ((market_data_type = 'date') = (market_data_date is not null)),
    check ("npv_enabled" in (0, 1)),
    check ("cashflow_enabled" in (0, 1)),
    check ("curves_enabled" in (0, 1)),
    check ("sensitivity_enabled" in (0, 1)),
    check ("simulation_enabled" in (0, 1)),
    check ("xva_enabled" in (0, 1)),
    check ("stress_enabled" in (0, 1)),
    check ("parametric_var_enabled" in (0, 1)),
    check ("initial_margin_enabled" in (0, 1)),
    check ("pfe_enabled" in (0, 1)),
    check ("xva_cva_enabled" in (0, 1)),
    check ("xva_dva_enabled" in (0, 1)),
    check ("xva_fva_enabled" in (0, 1)),
    check ("xva_colva_enabled" in (0, 1)),
    check ("xva_dim_enabled" in (0, 1)),
    check ("xva_dim_regression_order" is null or "xva_dim_regression_order" between 1 and 3),
    check ("var_method" is null or "var_method" in ('delta', 'delta_gamma_normal', 'monte_carlo'))
);

-- Unique report_definition_id for active records
create unique index if not exists risk_report_configs_report_definition_id_uniq_idx
on "ores_reporting_risk_report_configs_tbl" (tenant_id, report_definition_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists risk_report_configs_version_uniq_idx
on "ores_reporting_risk_report_configs_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists risk_report_configs_id_uniq_idx
on "ores_reporting_risk_report_configs_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists risk_report_configs_tenant_idx
on "ores_reporting_risk_report_configs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_risk_report_configs_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate report_definition_id (soft FK to ores_reporting_report_definitions_tbl)
    if not exists (
        select 1 from ores_reporting_report_definitions_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.report_definition_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid report_definition_id: %. No active report definition found with this id.', NEW.report_definition_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_reporting_risk_report_configs_tbl"
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
        update "ores_reporting_risk_report_configs_tbl"
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

create or replace trigger ores_reporting_risk_report_configs_insert_trg
before insert on "ores_reporting_risk_report_configs_tbl"
for each row execute function ores_reporting_risk_report_configs_insert_fn();

create or replace rule ores_reporting_risk_report_configs_delete_rule as
on delete to "ores_reporting_risk_report_configs_tbl" do instead (
    update "ores_reporting_risk_report_configs_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
