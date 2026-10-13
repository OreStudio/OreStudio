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
-- A catalogue of the providers whose market data the synthetic feeds simulate. A system-scope collection names the provider it stands for, so the installation can run a simulated Bloomberg, Refinitiv or CME once and every tenant reads it as that provider's data. Every collection names its provider, so every feed has one. A reference set such as the ORE examples is a provider of its own. Every provider here is simulated, and the synthetic screens show it without a mark, because everything on them is synthetic. Every source name a provider's feed publishes on starts synthetic.<provider code>., so a simulated feed is never read as the real one. A real provider integration, when one comes, belongs in market data and not in this catalogue. Scoped to ores.synthetic (not ores.refdata) because only synthetic generation uses it today, following the yield_curve_process_type precedent. Managed by the system tenant. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_market_data_providers_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "kind" text not null,
    "description" text not null,
    "display_order" integer not null default 0
);

create index if not exists dq_market_data_providers_artefact_dataset_idx
on ores_dq_market_data_providers_artefact_tbl (dataset_id);

create index if not exists dq_market_data_providers_artefact_tenant_idx
on ores_dq_market_data_providers_artefact_tbl (tenant_id);

create index if not exists dq_market_data_providers_artefact_code_idx
on ores_dq_market_data_providers_artefact_tbl (code);
