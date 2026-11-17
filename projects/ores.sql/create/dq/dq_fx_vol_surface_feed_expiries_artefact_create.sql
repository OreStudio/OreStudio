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
-- One expiry an fx_vol_surface_feed quotes. The feed orders its expiries by length, so the row carries no position. The forward variance of the segment that ends at this expiry, from the expiry before it, is a Cox-Ingersoll-Ross process with this row's reversion and volatility. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_fx_vol_surface_feed_expiries_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "feed_id" uuid not null,
    "tenor" text not null,
    "atm_vol" double precision not null,
    "variance_reversion" double precision not null,
    "variance_volatility" double precision not null
);

create index if not exists dq_fx_vol_surface_feed_expiries_artefact_dataset_idx
on ores_dq_fx_vol_surface_feed_expiries_artefact_tbl (dataset_id);

create index if not exists dq_fx_vol_surface_feed_expiries_artefact_tenant_idx
on ores_dq_fx_vol_surface_feed_expiries_artefact_tbl (tenant_id);

create index if not exists dq_fx_vol_surface_feed_expiries_artefact_id_idx
on ores_dq_fx_vol_surface_feed_expiries_artefact_tbl (id);

create index if not exists dq_fx_vol_surface_feed_expiries_artefact_feed_id_idx
on ores_dq_fx_vol_surface_feed_expiries_artefact_tbl (feed_id);
