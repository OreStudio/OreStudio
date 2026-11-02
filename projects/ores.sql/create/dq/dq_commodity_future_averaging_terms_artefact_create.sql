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
-- How an averaging commodity future averages the prices of its underlying, as ORE's AveragingData: the commodity it averages, the period, the pricing calendar, the conventions the averaging follows, whether it counts business days, the delivery roll days, the future month offset and the daily expiry offset. It is a part because its fields hold one invariant: each applies only when the future averages, and ORE requires the commodity, the period, the calendar and the conventions together, so the part makes those four columns required. A convention fills this part at most once. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_commodity_future_averaging_terms_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "convention_name" text not null,
    "kind_code" text not null,
    "averaging_commodity_name" text not null,
    "averaging_period" text not null,
    "averaging_pricing_calendar" text not null,
    "averaging_conventions" text not null,
    "averaging_use_business_days" boolean null,
    "averaging_delivery_roll_days" integer null,
    "averaging_future_month_offset" integer null,
    "averaging_daily_expiry_offset" integer null
);

create index if not exists dq_commodity_future_averaging_terms_artefact_dataset_idx
on ores_dq_commodity_future_averaging_terms_artefact_tbl (dataset_id);

create index if not exists dq_commodity_future_averaging_terms_artefact_tenant_idx
on ores_dq_commodity_future_averaging_terms_artefact_tbl (tenant_id);

create index if not exists dq_commodity_future_averaging_terms_artefact_convention_name_idx
on ores_dq_commodity_future_averaging_terms_artefact_tbl (convention_name);
