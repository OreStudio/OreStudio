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
-- Describes how ORE builds a constant-maturity-swap spread option: where its forward starts, how long its underlying swap runs, when it fixes, and the calendar and conventions its schedule rolls on. Corresponds to the <CmsSpreadOption> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). All eight fields are required and the corpus sets all eight in all twelve elements, which are otherwise identical: one convention shared by twelve files. The two period fields and the spot days stay in ORE's spelling, because they are tenors rather than frequency enumerations; the day count and the roll convention are normalised to the canonical codes the other tables hold. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_cms_spread_option_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "forward_start" text not null,
    "spot_days" text not null,
    "swap_tenor" text not null,
    "fixing_days" integer not null,
    "calendar" text not null,
    "day_count_fraction" text not null,
    "roll_convention" text not null,
    "oresmd_uri" text null
);

create index if not exists dq_cms_spread_option_conventions_artefact_dataset_idx
on ores_dq_cms_spread_option_conventions_artefact_tbl (dataset_id);

create index if not exists dq_cms_spread_option_conventions_artefact_tenant_idx
on ores_dq_cms_spread_option_conventions_artefact_tbl (tenant_id);

create index if not exists dq_cms_spread_option_conventions_artefact_id_idx
on ores_dq_cms_spread_option_conventions_artefact_tbl (id);
