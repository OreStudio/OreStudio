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
-- An account that can authenticate against the system: one row per user, service, algorithm or LLM identity, carrying the password material, the TOTP secret, the email address and the optional profile and reporting links. The table is bi-temporal and audited (see projects/ores.sql/create/iam/iam_accounts_create.sql): it carries version, the four audit columns and the valid_from/valid_to pair with the GIST exclusion, so the model takes the ordinary audited shape and needs no shape flag. The table is a composite parent: ores_iam_accounts_touch_version_fn lets a child entity (account contact information, party association) bump this account's own version when the child is written. The model declares :generate_touch_function: true, which renders that function under its existing name rather than leaving it hand-written. The model describes the table and nothing else. Two columns need care: - service_password_hash is a real column with no domain member: it is reached only by check_service_credentials and never travels on the wire, so it is declared :sql_only: true and the generated domain struct omits it while the entity struct and the mapper keep it. - image_id and reports_to_account_id are nullable UUID soft references. The hand-written domain struct represented both as a plain boost::uuids::uuid with a nil sentinel, on the claim that a second std::optional<boost::uuids::uuid> member corrupts reflect-cpp aggregate serialisation for multi-element vectors. Re-verified under the generated estate: all three nullable UUIDs are modelled as std::optional<boost::uuids::uuid>, and the api suite's multi-element JSON and table tests plus the core repository's five-account round trip pass, so the workaround is not needed here. The generated read surface is live, and it does not collide with the hand-written one. The hand-written account_operations_protocol.hpp owns the writes, iam.v1.accounts.{save,delete,update,lock,unlock,change-password,reset-password,select-party,set-default-party,switch-party,update-email,publish-from-dq}, and the generated account_protocol.hpp owns the reads, iam.v1.accounts.list and iam.v1.accounts.get, with the version reads alongside them. Both registrars are composed in ores.iam/core/src/messaging/registrar.cpp. This entity sets :read_only: true, so the generated half carries no write verb and the split falls out of the flag rather than out of a suppression. Two behavioural facets are switched off, each with a reason: - The entity's CRUD handler and sub-registrar, because the hand-written account_operations_handler already owns the write verbs. - The generated CRUD service, because the hand-written account_operations_service is the authentication surface (login, lock, unlock, password change and reset, party selection, service-credential check) and the generated service's get_account_history(id) collides in name and signature with the hand-written get_account_history(username) while meaning a different read. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_accounts_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "username" text not null,
    "full_name" text null,
    "email" text not null,
    "password_hash" text not null,
    "account_type" text not null default 'user',
    "business_unit_code" text null,
    "role" text null,
    "job_title" text null,
    "reports_to_username" text null,
    "photo_key" text null
);

create index if not exists dq_accounts_artefact_dataset_idx
on ores_dq_accounts_artefact_tbl (dataset_id);

create index if not exists dq_accounts_artefact_tenant_idx
on ores_dq_accounts_artefact_tbl (tenant_id);

create index if not exists dq_accounts_artefact_id_idx
on ores_dq_accounts_artefact_tbl (id);
