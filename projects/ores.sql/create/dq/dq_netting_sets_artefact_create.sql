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
-- A netting set is the group of trades with one counterparty whose values offset one another when credit exposure is computed. Every trade names exactly one set, as ORE requires; a trade with nothing to net against sits in a set of its own. See [[id:5F12A37F-9F26-4B49-BAAF-1BAA7B2BB94F][Netting sets]] for the analysis. A set opened under a [[id:6AADB1A7-1229-4D10-8951-58446788957F][netting agreement]] copies the agreement's counterparty and legal entity, and a pinned key holds the copy to the agreement, so a set cannot be filed under another counterparty's agreement. A set with no agreement holds trades that do not net; its counterparty may be unknown, as it is for a set read from an ORE document, which names none. Every set belongs to a legal entity of the firm, and the entity that imports the document is the one it belongs to. The code is the netting set id ORE uses. The call type and initial margin type are the remaining parts of ORE's netting set details, and the risk weight is ORE's. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_netting_sets_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "agreement_number" text null,
    "counterparty_lei" text null,
    "call_type" text null,
    "initial_margin_type" text null,
    "risk_weight" double precision null,
    "description" text null
);

create index if not exists dq_netting_sets_artefact_dataset_idx
on ores_dq_netting_sets_artefact_tbl (dataset_id);

create index if not exists dq_netting_sets_artefact_tenant_idx
on ores_dq_netting_sets_artefact_tbl (tenant_id);

create index if not exists dq_netting_sets_artefact_code_idx
on ores_dq_netting_sets_artefact_tbl (code);

create index if not exists dq_netting_sets_artefact_agreement_number_idx
on ores_dq_netting_sets_artefact_tbl (agreement_number);
