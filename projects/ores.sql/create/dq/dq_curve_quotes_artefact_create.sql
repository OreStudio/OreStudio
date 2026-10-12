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
-- One market quote a curve is built from. A quote sits either directly on the curve entry or inside one of its segments, or in a default curve's configuration, and the row names the curve entry as well so the quotes of a curve can be read without walking its segments or configurations. Almost every list is of Quote elements, each a market point and an optional optional attribute. An average OIS segment lists CompositeQuote elements instead, each a rate quote and a spread quote. A row is one or the other: a quote_text, or a rate_quote with a spread_quote. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_curve_quotes_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "curve_segment_id" uuid null,
    "default_curve_configuration_id" uuid null,
    "commodity_price_segment_id" uuid null,
    "quote_list" text null,
    "quote_text" text null,
    "optional_flag" text null,
    "rate_quote" text null,
    "spread_quote" text null,
    "position" integer not null
);

create index if not exists dq_curve_quotes_artefact_dataset_idx
on ores_dq_curve_quotes_artefact_tbl (dataset_id);

create index if not exists dq_curve_quotes_artefact_tenant_idx
on ores_dq_curve_quotes_artefact_tbl (tenant_id);

create index if not exists dq_curve_quotes_artefact_id_idx
on ores_dq_curve_quotes_artefact_tbl (id);
