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
-- The equity underlyings a population may draw from. An equity instrument states underlying_name, documented as the ORE equity Name identifier, and nothing held the set a tenant may use, so a generator that needed one had to invent it. The code is the name the engine resolves, not a display name. ORE's own example trades price underlyings as bare index names such as .SPX and .STOXX50E, as exchange-suffixed shares such as MSFT.O and CR.N, and as ISINs such as GB0005405286. The catalogue therefore stores the spelling a document states, because that is the string the engine resolves. The same instrument appears under more than one spelling: .SPX and RIC:.SPX are one index, and the catalogue holds both because a document may state either. Resolving them to one instrument is what resolves_to is for: a row that is the instrument leaves it empty, and a row that is an alias names the instrument it resolves to. A population that read the two as two instruments would price one index twice. The kind travels with the underlying because it decides what the trade is. An option on an index and an option on a share are the same instrument type in the store, but a book that claimed to trade single names while holding only indices would misreport itself. Only underlyings a document states as Equity belong here. ORE states each underlying's kind beside its name, and one corpus trade proves the kind is not implied by the trade type: an EquityWorstOfBasketSwap in the examples carries two underlyings whose kind is FX, total return indices on currency pairs. A catalogue that took every name an equity typed trade mentions would file a currency pair as an equity. This model holds the vocabulary only. The prices and the volatility surfaces an equity prices on are market data, and they are not here. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_equity_underlyings_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "instrument_type" text not null,
    "resolves_to" text null,
    "currency_code" text null,
    "display_order" integer not null default 0
);

create index if not exists dq_equity_underlyings_artefact_dataset_idx
on ores_dq_equity_underlyings_artefact_tbl (dataset_id);

create index if not exists dq_equity_underlyings_artefact_tenant_idx
on ores_dq_equity_underlyings_artefact_tbl (tenant_id);

create index if not exists dq_equity_underlyings_artefact_code_idx
on ores_dq_equity_underlyings_artefact_tbl (code);
