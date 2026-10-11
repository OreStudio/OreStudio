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
-- Contact details for parties organised by purpose (Legal, Operations, Settlement, Billing). Each party can have one contact record per type. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_party_contact_informations_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "contact_type" text not null,
    "version" integer not null,
    "street_line_1" text null,
    "street_line_2" text null,
    "city" text null,
    "state" text null,
    "country_code" text null,
    "postal_code" text null,
    "phone" text null,
    "email" text null,
    "web_page" text null,
    "is_primary" boolean not null
);

create index if not exists dq_party_contact_informations_artefact_dataset_idx
on ores_dq_party_contact_informations_artefact_tbl (dataset_id);

create index if not exists dq_party_contact_informations_artefact_tenant_idx
on ores_dq_party_contact_informations_artefact_tbl (tenant_id);

create index if not exists dq_party_contact_informations_artefact_contact_type_idx
on ores_dq_party_contact_informations_artefact_tbl (contact_type);
