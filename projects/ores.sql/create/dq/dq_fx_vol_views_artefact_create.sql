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
-- A view is one vendor's quotes of a latent FX volatility surface: the surface, which no vendor publishes as its own, with the vendor's noise over it. It is a feed of the vendor's collection, named in the vendor's namespace, such as synthetic.bloomberg.fx.fx_option.audusd, and listed, counted, stopped and started with the vendor's other feeds. It reads two inputs: the latent surface it quotes, and the vendor's noise profile it applies, which is one per vendor and shared by every pair the vendor quotes. The vendor's simulated source runs it, holding the shared surface and publishing the view's points under the vendor's ids. A view of the clean demo contributor applies no noise: it records the latent surface itself, for the tests to compare against. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_fx_vol_views_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "feed_id" uuid not null,
    "contributor_id" uuid not null
);

create index if not exists dq_fx_vol_views_artefact_dataset_idx
on ores_dq_fx_vol_views_artefact_tbl (dataset_id);

create index if not exists dq_fx_vol_views_artefact_tenant_idx
on ores_dq_fx_vol_views_artefact_tbl (tenant_id);

create index if not exists dq_fx_vol_views_artefact_id_idx
on ores_dq_fx_vol_views_artefact_tbl (id);

create index if not exists dq_fx_vol_views_artefact_feed_id_idx
on ores_dq_fx_vol_views_artefact_tbl (feed_id);

create index if not exists dq_fx_vol_views_artefact_contributor_id_idx
on ores_dq_fx_vol_views_artefact_tbl (contributor_id);
