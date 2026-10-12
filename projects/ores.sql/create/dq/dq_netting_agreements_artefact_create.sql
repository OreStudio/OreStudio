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
-- A netting agreement is the legal document, such as an ISDA Master Agreement, that lets one legal entity of the firm and one counterparty replace what each owes the other under it with a single net amount. It is the legal fact behind a [[id:5F12A37F-9F26-4B49-BAAF-1BAA7B2BB94F][netting set]]: a set opened under an agreement belongs to the agreement's two parties, and a set with no agreement holds trades that do not net. The agreement's two parties never change across its versions: both columns are fixed, so a new version that changes either is refused. A netting set copies them and pins the copy to the agreement, so a set cannot be filed under another counterparty's agreement, and the copy stays true. Closing an agreement does not close its sets, as for every soft foreign key in the schema; a set's agreement is checked when the set is written. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_netting_agreements_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "agreement_number" text not null,
    "version" integer not null,
    "counterparty_lei" text not null,
    "agreement_type" text not null,
    "governing_law" text null,
    "description" text null
);

create index if not exists dq_netting_agreements_artefact_dataset_idx
on ores_dq_netting_agreements_artefact_tbl (dataset_id);

create index if not exists dq_netting_agreements_artefact_tenant_idx
on ores_dq_netting_agreements_artefact_tbl (tenant_id);

create index if not exists dq_netting_agreements_artefact_agreement_number_idx
on ores_dq_netting_agreements_artefact_tbl (agreement_number);

create index if not exists dq_netting_agreements_artefact_counterparty_lei_idx
on ores_dq_netting_agreements_artefact_tbl (counterparty_lei);
