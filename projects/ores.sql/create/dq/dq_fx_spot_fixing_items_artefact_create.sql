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
-- One FX fixing that an fx_spot_generation_config whose price_source is fixing may start from. A feed reads its items in rank order and starts from the first that has a fixing: the latest on or before the date of the market clock, or on a stated date. The window sets how many days of fixings measure the volatility. An item may quote the pair the other way round, and the feed then reads it as one over the value. Party- and tenant-scoped. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_fx_spot_fixing_items_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "rank" integer not null,
    "item" text not null,
    "provider_code" text null,
    "policy" text not null,
    "window_days" integer not null
);

create index if not exists dq_fx_spot_fixing_items_artefact_dataset_idx
on ores_dq_fx_spot_fixing_items_artefact_tbl (dataset_id);

create index if not exists dq_fx_spot_fixing_items_artefact_tenant_idx
on ores_dq_fx_spot_fixing_items_artefact_tbl (tenant_id);

create index if not exists dq_fx_spot_fixing_items_artefact_base_currency_code_idx
on ores_dq_fx_spot_fixing_items_artefact_tbl (base_currency_code);

create index if not exists dq_fx_spot_fixing_items_artefact_pair_idx
on ores_dq_fx_spot_fixing_items_artefact_tbl (dataset_id, base_currency_code, quote_currency_code);
