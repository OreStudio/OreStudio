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
 * Risk Report Configurations Table
 *
 * Stores the ORE-level parameters for a risk report definition. Each row is
 * owned by exactly one report_definition (1:1 relationship, enforced by the
 * unique index on report_definition_id).
 *
 * Analytics flags use integer 0/1 (not boolean) to match the project
 * convention. npv and cashflow default to enabled; all others default to off.
 *
 * Market data convention is split into market_data_type ('live'|'eod'|'date')
 * and market_data_date (set only when type = 'date'), enforced by a CHECK.
 *
 * Portfolio and book scope are stored in separate junction tables. An empty
 * set in either junction table means "all visible to the tenant".
 */

create table if not exists "ores_reporting_risk_report_configs_tbl" (
    "id"                              uuid       not null,
    "tenant_id"                       uuid       not null,
    "version"                         integer    not null,
    "report_definition_id"            uuid       not null,

    -- Core ORE setup
    "base_currency"                   text       not null,
    "observation_model"               text       not null default 'disable',
    "n_threads"                       integer    not null default 1,

    -- Market data convention
    "market_data_type"                text       not null default 'eod',
    "market_data_date"                date       null,

    -- Analytics flags (1 = enabled, 0 = disabled)
    "npv_enabled"                     integer    not null default 1,
    "cashflow_enabled"                integer    not null default 1,
    "curves_enabled"                  integer    not null default 0,
    "sensitivity_enabled"             integer    not null default 0,
    "simulation_enabled"              integer    not null default 0,
    "xva_enabled"                     integer    not null default 0,
    "stress_enabled"                  integer    not null default 0,
    "parametric_var_enabled"          integer    not null default 0,
    "initial_margin_enabled"          integer    not null default 0,
    "pfe_enabled"                     integer    not null default 0,

    -- XVA settings (relevant when xva_enabled = 1)
    "xva_quantile"                    numeric(5,4) null,
    "xva_cva_enabled"                 integer    not null default 1,
    "xva_dva_enabled"                 integer    not null default 0,
    "xva_fva_enabled"                 integer    not null default 0,
    "xva_colva_enabled"               integer    not null default 0,
    "xva_dim_enabled"                 integer    not null default 0,
    "xva_dim_quantile"                numeric(5,4) null,
    "xva_dim_horizon_calendar_days"   integer    null,
    "xva_dim_regression_order"        integer    null,

    -- Parametric VaR settings (relevant when parametric_var_enabled = 1)
    "var_quantiles"                   numeric(5,4)[] null,
    "var_method"                      text       null,

    -- SIMM / Initial Margin settings (relevant when initial_margin_enabled = 1)
    "simm_version"                    text       null,
    "simm_calculation_currency"       text       null,

    "modified_by"                     text       not null,
    "performed_by"                    text       not null,
    "change_reason_code"              text       not null,
    "change_commentary"               text       not null,
    "valid_from"                      timestamp with time zone not null,
    "valid_to"                        timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
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

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_reporting_risk_report_configs_version_uniq_idx
on "ores_reporting_risk_report_configs_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Current record uniqueness
create unique index if not exists ores_reporting_risk_report_configs_id_uniq_idx
on "ores_reporting_risk_report_configs_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- One config per report definition (enforced on current records)
create unique index if not exists ores_reporting_risk_report_configs_definition_uniq_idx
on "ores_reporting_risk_report_configs_tbl" (tenant_id, report_definition_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Tenant index
create index if not exists ores_reporting_risk_report_configs_tenant_idx
on "ores_reporting_risk_report_configs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_risk_report_configs_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Validate report_definition_id (soft FK)
    if not exists (
        select 1 from ores_reporting_report_definitions_tbl
        where tenant_id = new.tenant_id
          and id = new.report_definition_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid report_definition_id: %. No active report definition found with this id.',
            new.report_definition_id
            using errcode = '23503';
    end if;

    -- Version management
    select version into current_version
    from "ores_reporting_risk_report_configs_tbl"
    where tenant_id = new.tenant_id
      and id = new.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "ores_reporting_risk_report_configs_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
          and id = new.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_actor_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql security definer set search_path = public;

create or replace trigger ores_reporting_risk_report_configs_insert_trg
before insert on "ores_reporting_risk_report_configs_tbl"
for each row execute function ores_reporting_risk_report_configs_insert_fn();

create or replace rule ores_reporting_risk_report_configs_delete_rule as
on delete to "ores_reporting_risk_report_configs_tbl" do instead
    update "ores_reporting_risk_report_configs_tbl"
    set valid_to = current_timestamp
    where tenant_id = old.tenant_id
      and id = old.id
      and valid_to = ores_utility_infinity_timestamp_fn();
