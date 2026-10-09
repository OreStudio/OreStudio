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
-- Describes how ORE builds a bond yield: the compounding it is quoted on, the frequency it pays at, whether the price is clean or dirty, and the tolerances its solver runs to. Corresponds to the <BondYield> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). One field is required and six are optional. One file carries five elements and sets all six optional fields. The accuracy and the guess are floats in ORE and travel through a double column, because the refdata schema has no float type; a float widens to a double and narrows back with no loss. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_bond_yield_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "compounding" text not null,
    "frequency" text null,
    "price_type" text null,
    "accuracy" double precision null,
    "max_evaluations" integer null,
    "guess" double precision null,
    "oresmd_uri" text null
);

create index if not exists dq_bond_yield_conventions_artefact_dataset_idx
on ores_dq_bond_yield_conventions_artefact_tbl (dataset_id);

create index if not exists dq_bond_yield_conventions_artefact_tenant_idx
on ores_dq_bond_yield_conventions_artefact_tbl (tenant_id);

create index if not exists dq_bond_yield_conventions_artefact_id_idx
on ores_dq_bond_yield_conventions_artefact_tbl (id);
