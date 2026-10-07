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
-- An account's address/phone/email/web page fields. The account's real name lives on ores_iam_accounts_tbl itself (full_name), not here — this entity is purely "how to reach them", not "who they are". One contact record per account (unlike party contact information, which allows several by contact_type — a person doesn't need a Legal/ Operations/Settlement/Billing split). The record is personal data, so the generated reads are not open to every signed-in caller: iam.v1.account_contact_informations.list, get and list_by_account_id require iam::account_contact_informations:read, as every generated read requires its resource's read code. A person reads their own record through iam.v1.ops.get_my_account_contact_information, which takes no account id, and writes it through update-self; both are in [[id:082C5D76-93C2-41E6-8193-9598B79DD07A][ores.iam.account_messages]]. See [[id:804C7048-DBBF-4B39-8737-BFB4949884C4][Authorised reads]]. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_account_contact_informations_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "account_username" text not null,
    "street_line_1" text null,
    "street_line_2" text null,
    "city" text null,
    "state" text null,
    "country_code" text null,
    "postal_code" text null,
    "phone" text null,
    "email" text null,
    "web_page" text null
);

create index if not exists dq_account_contact_informations_artefact_dataset_idx
on ores_dq_account_contact_informations_artefact_tbl (dataset_id);

create index if not exists dq_account_contact_informations_artefact_tenant_idx
on ores_dq_account_contact_informations_artefact_tbl (tenant_id);

create index if not exists dq_account_contact_informations_artefact_id_idx
on ores_dq_account_contact_informations_artefact_tbl (id);
