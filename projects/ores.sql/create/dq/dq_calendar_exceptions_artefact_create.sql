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
-- One row per one-off override for a single [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]] day. Mirrors ores::analytics::quant::domain::calendar_exception plus the usual refdata plumbing. Applies uniformly to *any* calendar -- transcribed or user-authored, based or base-less: a based calendar's full override story is entirely calendar.base_calendar_code (which base) plus its own exception rows (what's different), no separate "adjustment" concept needed. QuantLib's own irregular, year-gated special dates (the UK's Jubilee bank holidays, the NYSE's special closings) are transcribed here, not folded into a [[id:875E96F6-3FC7-4E0B-8E3C-2AC0F8BD488F][calendar_rule]] row, so a rule stays a timeless, indefinitely-repeating pattern and the genuinely irregular part of a calendar stays flat, no-logic-at-all data. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_calendar_exceptions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "calendar_code" text not null,
    "exception_date" date not null,
    "is_business_day" boolean not null default false,
    "description" text null
);

create index if not exists dq_calendar_exceptions_artefact_dataset_idx
on ores_dq_calendar_exceptions_artefact_tbl (dataset_id);

create index if not exists dq_calendar_exceptions_artefact_tenant_idx
on ores_dq_calendar_exceptions_artefact_tbl (tenant_id);

create index if not exists dq_calendar_exceptions_artefact_id_idx
on ores_dq_calendar_exceptions_artefact_tbl (id);
