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
-- A master netting agreement, such as an ISDA Master Agreement, between the party a publish targets and a counterparty. The counterparty is named by LEI, because its id differs from tenant to tenant, and the party is the one the publish names: a staged agreement belongs to no party until published. - Artefact Table
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
