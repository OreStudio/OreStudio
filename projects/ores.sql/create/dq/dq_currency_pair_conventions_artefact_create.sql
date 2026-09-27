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
-- Quoting and date-convention fields for a currency pair — pip factor, tick size, calendars, business day convention, spot-relative/end-of- month flags — folded in from the retired fx_convention entity (see [[id:04A121FA-00D6-43EB-9B21-04EDC1FA493D][Currency pair support in reference data]]). Keyed 1:1 by pair_code, the same value space as [[id:E1EE950D-FC22-4DAB-A93F-8B2A15196031][ores.refdata.currency_pair]]'s own primary key — every pair has at most one convention record, so a separate identifier scheme would be pure overhead. Like every other soft-FK relationship in this codebase, pair_code is validated via trigger, not a hard DB foreign key. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_currency_pair_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "pair_code" text not null,
    "version" integer not null,
    "pip_factor" double precision not null,
    "tick_size" double precision not null,
    "decimal_places" integer not null,
    "advance_calendar" text null,
    "business_day_convention" text null,
    "spot_relative" boolean null,
    "end_of_month" boolean null
);

create index if not exists dq_currency_pair_conventions_artefact_dataset_idx
on ores_dq_currency_pair_conventions_artefact_tbl (dataset_id);

create index if not exists dq_currency_pair_conventions_artefact_tenant_idx
on ores_dq_currency_pair_conventions_artefact_tbl (tenant_id);

create index if not exists dq_currency_pair_conventions_artefact_pair_code_idx
on ores_dq_currency_pair_conventions_artefact_tbl (pair_code);
