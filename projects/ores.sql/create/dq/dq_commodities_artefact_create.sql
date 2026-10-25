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
-- The commodities a population may draw from. A commodity trade names its commodity in free text today, and nothing holds the set a tenant may use, so a generator that needs one has to invent it. The code is the name the engine resolves, not a display name: ORE's own index corpus and example trades spell a commodity as an exchange-prefixed index, such as NYMEX:CL, ICE:B, XLME:AH or PM:XAUUSD. The catalogue therefore stores that spelling as its key. The unit and the quotation currency travel with the commodity because they are facts about how it trades and not about a particular trade: WTI is quoted in barrels for dollars, gas in MMBtu for dollars, LME aluminium in tonnes, gold in ounces. A population that quoted gold per barrel would be worse than one with no commodity at all. This model holds the vocabulary only. The forward curves a commodity prices on are market data, and they are not here. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_commodities_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "unit_code" text not null,
    "quotation_currency_code" text not null,
    "display_order" integer not null default 0
);

create index if not exists dq_commodities_artefact_dataset_idx
on ores_dq_commodities_artefact_tbl (dataset_id);

create index if not exists dq_commodities_artefact_tenant_idx
on ores_dq_commodities_artefact_tbl (tenant_id);

create index if not exists dq_commodities_artefact_code_idx
on ores_dq_commodities_artefact_tbl (code);
