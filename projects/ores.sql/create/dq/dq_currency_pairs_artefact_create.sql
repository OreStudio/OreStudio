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
-- Currency pair identity: base/quote legs, deliverability, classification, and fixing source. Conventions (pip factor, tick size, calendars, business day convention) live 1:1 in [[id:1B88215B-1FE0-4CAF-B6AB-53F471963CA6][ores.refdata.currency_pair_convention]], matching the codebase's existing *_convention entity family. spot_days, calendars, and G11 membership are *derived* at read time from the two legs, not stored here — see [[id:04A121FA-00D6-43EB-9B21-04EDC1FA493D][Currency pair support in reference data]] for the full design rationale. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_currency_pairs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "pair_code" text not null,
    "version" integer not null,
    "base_currency" text not null,
    "quote_currency" text not null,
    "classification" text not null
);

create index if not exists dq_currency_pairs_artefact_dataset_idx
on ores_dq_currency_pairs_artefact_tbl (dataset_id);

create index if not exists dq_currency_pairs_artefact_tenant_idx
on ores_dq_currency_pairs_artefact_tbl (tenant_id);

create index if not exists dq_currency_pairs_artefact_pair_code_idx
on ores_dq_currency_pairs_artefact_tbl (pair_code);
