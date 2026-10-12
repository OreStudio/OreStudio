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
 * Template: sql_schema_domain_entity_artefact_create.mustache
 * To modify, update the template and regenerate.
 */

-- =============================================================================
-- The ORE-level run settings that risk report definitions of a party share: base currency, observation model, analytics flags, XVA, VaR and SIMM settings and threading. A report definition binds the configuration through report_configurations, in the slot of configuration type risk_report_config, as it binds its pricing engines. The analytics it runs, and their parameters, are rows of configuration_analytic and configuration_analytic_parameter. Portfolio and book scope live in separate temporal junction tables keyed by this row. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_risk_report_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "name" text not null,
    "description" text null,
    "configuration_id" uuid not null,
    "base_currency" text null,
    "observation_model" text null,
    "n_threads" integer null,
    "market_data_type" text not null,
    "market_data_date" date null,
    "accrual_date" text null,
    "fixing_cutoff" text null,
    "input_path" text not null,
    "input_path_market" text null,
    "input_path_portfolio" text null,
    "output_path" text not null,
    "log_file" text not null,
    "date_calendar" text null,
    "date_convention" text null,
    "continue_on_error" text null,
    "build_failed_trades" text null,
    "imply_todays_fixings" text null,
    "include_todays_cash_flows" text null,
    "include_reference_date_events" text null,
    "lazy_market_building" text null,
    "enrich_index_fixings" text null,
    "use_analytics" text null,
    "csv_comment_report_header" text null,
    "default_mapping_to_identity" text null,
    "portfolio_recurse_into_sub_directories" text null,
    "log_mask" integer null,
    "ignore_fixing_lag" integer null,
    "additional_results" text null,
    "scope_portfolio" text null
);

create index if not exists dq_risk_report_configs_artefact_dataset_idx
on ores_dq_risk_report_configs_artefact_tbl (dataset_id);

create index if not exists dq_risk_report_configs_artefact_tenant_idx
on ores_dq_risk_report_configs_artefact_tbl (tenant_id);

create index if not exists dq_risk_report_configs_artefact_id_idx
on ores_dq_risk_report_configs_artefact_tbl (id);
