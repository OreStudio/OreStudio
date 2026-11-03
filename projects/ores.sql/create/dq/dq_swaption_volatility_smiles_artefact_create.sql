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
-- ORE's swaptionVolatility is one xs:all with an optional Dimension and every smile element optional. ORE reads the three smile lists only for a Smile surface, and they are one block: in the ORE examples an entry states all three or none. A row of this part is that block, and it exists wherever the entry states one, as the FX smile part does for the FX entry. The examples also keep a grid on three ATM entries whose dimension was switched from Smile, so the part does not depend on the dimension, and the document round trips as written. The part holds SmileOptionTenors, SmileSwapTenors and SmileSpreads, each a comma separated list in ORE's own text. Dimension stays on the entry, because it is the entry's own setting and the smile only depends on it. An entry holds at most one row of this part, so the generated table adds the unique index on the live rows of (tenant_id, swaption_volatility_config_id). - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_swaption_volatility_smiles_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "swaption_volatility_config_id" uuid not null,
    "smile_option_tenors" text null,
    "smile_swap_tenors" text null,
    "smile_spreads" text null
);

create index if not exists dq_swaption_volatility_smiles_artefact_dataset_idx
on ores_dq_swaption_volatility_smiles_artefact_tbl (dataset_id);

create index if not exists dq_swaption_volatility_smiles_artefact_tenant_idx
on ores_dq_swaption_volatility_smiles_artefact_tbl (tenant_id);

create index if not exists dq_swaption_volatility_smiles_artefact_id_idx
on ores_dq_swaption_volatility_smiles_artefact_tbl (id);
