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
-- Describes how ORE builds a commodity forward: how many days after the trade it starts, how its points are scaled, the calendar it advances on, and whether it is quoted outright or as a spread to spot. Corresponds to the <CommodityForward> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). One field is required and seven are optional. Two files carry five elements between them, and every one sets all seven optional fields except the delivery location, which none sets. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_commodity_forward_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "spot_days" integer null,
    "points_factor" double precision null,
    "advance_calendar" text null,
    "spot_relative" boolean null,
    "delivery_location" text null,
    "business_day_convention" text null,
    "outright" boolean null,
    "oresmd_uri" text null
);

create index if not exists dq_commodity_forward_conventions_artefact_dataset_idx
on ores_dq_commodity_forward_conventions_artefact_tbl (dataset_id);

create index if not exists dq_commodity_forward_conventions_artefact_tenant_idx
on ores_dq_commodity_forward_conventions_artefact_tbl (tenant_id);

create index if not exists dq_commodity_forward_conventions_artefact_id_idx
on ores_dq_commodity_forward_conventions_artefact_tbl (id);
