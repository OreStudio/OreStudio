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
-- One contributor's quote of a pillar: the market series it is read from, and the weight the contributor carries in the calibration. A pillar reads one series per contributor, so a broker's quotes and the derived source's quotes of the same point are two sources of one pillar, and a missing observation from one of them is not a refusal by itself. See [[id:7AE57098-08D4-4D4A-83D6-918FE520B68E][A pillar names the market series it reads]]. market_series_id is a soft reference to an fx_option quote series in ores.marketdata, named by the caller from the series the party holds. contributor_code names the provider whose series it is, as the series' own provider_code does, so a reader sees the contributor without a cross-component read. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_fx_vol_surface_pillar_sources_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "pillar_id" uuid not null,
    "market_series_id" uuid not null,
    "contributor_code" text not null,
    "weight" double precision not null default 1.0
);

create index if not exists dq_fx_vol_surface_pillar_sources_artefact_dataset_idx
on ores_dq_fx_vol_surface_pillar_sources_artefact_tbl (dataset_id);

create index if not exists dq_fx_vol_surface_pillar_sources_artefact_tenant_idx
on ores_dq_fx_vol_surface_pillar_sources_artefact_tbl (tenant_id);

create index if not exists dq_fx_vol_surface_pillar_sources_artefact_id_idx
on ores_dq_fx_vol_surface_pillar_sources_artefact_tbl (id);
