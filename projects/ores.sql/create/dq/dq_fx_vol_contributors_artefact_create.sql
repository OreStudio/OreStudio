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
-- A synthetic contributor, a provider of the simulator that quotes FX volatility as a broker does. Its noise profile is its own, whatever pair it quotes, so it is held once here and fx_vol_surface_feed_contributor says which pairs it quotes. A profile of zeros is the clean contributor, whose quotes are the truth. The noise of a point is noise, widened on the wings by 1 + wing_widening * (0.5 - delta) and for long expiries by 1 + expiry_widening * (years - 1) past one year. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_fx_vol_contributors_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "provider_code" text not null,
    "noise" double precision not null,
    "wing_widening" double precision not null,
    "expiry_widening" double precision not null,
    "quote_step" double precision not null,
    "missing_probability" double precision not null,
    "stale_probability" double precision not null,
    "outlier_probability" double precision not null,
    "outlier_size" double precision not null,
    "bias" double precision not null
);

create index if not exists dq_fx_vol_contributors_artefact_dataset_idx
on ores_dq_fx_vol_contributors_artefact_tbl (dataset_id);

create index if not exists dq_fx_vol_contributors_artefact_tenant_idx
on ores_dq_fx_vol_contributors_artefact_tbl (tenant_id);

create index if not exists dq_fx_vol_contributors_artefact_id_idx
on ores_dq_fx_vol_contributors_artefact_tbl (id);

create index if not exists dq_fx_vol_contributors_artefact_provider_code_idx
on ores_dq_fx_vol_contributors_artefact_tbl (provider_code);
