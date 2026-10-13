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
-- A top-level container that owns the [[id:1907531F-E2AF-4BF7-84A4-6D69CB9EDFD7][spanning-tree topology]]'s pivot currency and the set of [[id:B38B3869-02FD-4CC7-99BD-9A77904ACA19][CRM]] driver pairs (crm_driver_pair, a separate, config_id-referencing entity) that hang off it. ores.marketdata.service reads the enabled config per (tenant, party) and its enabled driver pairs, feeds them through ores.analytics.quant::topology_builder::build, and keeps one service::rate_engine per (tenant, party) up to date -- see [[file:../../../doc/agile/versions/v0/sprint_23/crm_implementation/task_wire_crm_into_marketdata_ingest.org][the wiring task]]. UI metadata is deferred to a later task, as it is for market_series. Scoped to a tenant and a party so each party runs its own, independent CRM -- two parties in the same tenant may have entirely different topologies and never share an engine. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_crm_topology_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "crm_name" text not null,
    "pivot_currency_code" text not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "row_kind" text not null
    ,check ("row_kind" in ('driver', 'derived'))
    ,check ("base_currency_code" <> '')
    ,check ("quote_currency_code" <> '')
    ,check ("base_currency_code" <> "quote_currency_code")
    ,check ("crm_name" <> '')
    ,check ("pivot_currency_code" <> '')
);

create index if not exists dq_crm_topology_configs_artefact_dataset_idx
on ores_dq_crm_topology_configs_artefact_tbl (dataset_id);

create index if not exists dq_crm_topology_configs_artefact_tenant_idx
on ores_dq_crm_topology_configs_artefact_tbl (tenant_id);

create index if not exists dq_crm_topology_configs_artefact_crm_name_idx
on ores_dq_crm_topology_configs_artefact_tbl (crm_name);

create index if not exists dq_crm_topology_configs_artefact_name_idx
on ores_dq_crm_topology_configs_artefact_tbl (dataset_id, crm_name);
