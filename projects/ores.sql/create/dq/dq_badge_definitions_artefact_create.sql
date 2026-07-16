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
-- =============================================================================
-- Badge Definitions Artefact Table
--
-- Staging table for badge definitions before publication into a tenant's own
-- copy of dq_badge_definitions_tbl. Supports the DQ artefact pipeline pattern
-- (self-referential: DQ publishing into its own table, like coding_schemes
-- and badge_severities).
-- =============================================================================

create table if not exists "ores_dq_badge_definitions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null default 0,
    "name" text not null,
    "description" text not null,
    "background_colour" text not null,
    "text_colour" text not null,
    "severity_code" text not null,
    "css_class" text,
    "display_order" integer not null
);

create index if not exists badge_definitions_artefact_dataset_idx
on "ores_dq_badge_definitions_artefact_tbl" (dataset_id);

create index if not exists badge_definitions_artefact_tenant_idx
on "ores_dq_badge_definitions_artefact_tbl" (tenant_id);

create index if not exists badge_definitions_artefact_code_idx
on "ores_dq_badge_definitions_artefact_tbl" (code);
