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
-- A typed sub-configuration owned by a market_data_generation_config. Describes how to synthesise a single currency+index interest-rate curve: which short-rate process drives it (vasicek/cox_ingersoll_ross/hull_white/ two_factor_gaussian, per ores.analytics.quant's i_yield_curve_process engines) and that process's parameters, stored row-based as ir_curve_generation_config_process_parameter_value children joined onto the yield_curve_process_parameter_definition catalogue -- so a new process type or parameter is seed data, not a schema change. The actual instrument grid (which tenors to publish at which role) is held separately as ordered ir_curve_template_entry rows, the same one-config-many-children shape fx_spot_generation_config uses for its gmm_component rows. Scoped to a tenant and a party. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_ir_curve_generation_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text null,
    "enabled" boolean not null,
    "auto_start" boolean not null default false,
    "currency_code" text not null,
    "index_family" text not null,
    "tenor" text not null default '',
    "process_type" text not null,
    "kappa" double precision not null,
    "theta" double precision not null,
    "sigma" double precision not null,
    "initial_rate" double precision not null,
    "ticks_per_hour" integer not null,
    "fixed_leg_payment_frequency_code" text not null,
    "price_source" text not null default 'fixed',
    "provider_code" text null,
    "arrival_mode" text not null default 'periodic',
    "branching_ratio" double precision not null default 0.0,
    "decay" double precision not null default 1.0,
    "sensitivity" double precision not null default 1.0
);

create index if not exists dq_ir_curve_generation_configs_artefact_dataset_idx
on ores_dq_ir_curve_generation_configs_artefact_tbl (dataset_id);

create index if not exists dq_ir_curve_generation_configs_artefact_tenant_idx
on ores_dq_ir_curve_generation_configs_artefact_tbl (tenant_id);

create index if not exists dq_ir_curve_generation_configs_artefact_id_idx
on ores_dq_ir_curve_generation_configs_artefact_tbl (id);

create index if not exists dq_ir_curve_generation_configs_artefact_currency_code_idx
on ores_dq_ir_curve_generation_configs_artefact_tbl (currency_code);
