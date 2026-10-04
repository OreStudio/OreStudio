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
-- One row per dated diary entry on a [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]]: a central-bank meeting, a scheduled data release, or an open-ended other event. One table for all event kinds -- never a table per calendar or per type (settled 2026-08-09, story Decision D1; see [[id:41E0E1FB-1D84-47E0-A417-2633F31F0A2A][Calendar Events]]). The diary_entry_type column classifies the entry via the open-ended [[id:AFAF296D-2962-48CE-A6E1-BFD5229E16C5][diary_entry_type]] vocabulary (holiday, central_bank_meeting, data_release, other). Holidays themselves keep their existing machinery (calendar_rules, calendar_exceptions, calendar_date) -- the holiday type stays in the vocabulary so the whole classification lives in one place, even though its physical home is elsewhere. Template/Instance: an event row is an *instance*; its template is the (calendar, diary_entry_type, name) triple. A worked case: the FOMC's eight regularly scheduled meetings per year are entered as a short run of central_bank_meeting instances on the US.FOMC calendar, transcribed from the Fed's published calendar with source='federalreserve.gov'. Formulaic recurrence generation is deferred; calendar_rules's grammar can later feed a template link if a consumer needs it. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_calendar_events_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "calendar_code" text not null,
    "event_date" date not null,
    "diary_entry_type" text not null,
    "name" text not null,
    "description" text null,
    "source" text null
);

create index if not exists dq_calendar_events_artefact_dataset_idx
on ores_dq_calendar_events_artefact_tbl (dataset_id);

create index if not exists dq_calendar_events_artefact_tenant_idx
on ores_dq_calendar_events_artefact_tbl (tenant_id);

create index if not exists dq_calendar_events_artefact_id_idx
on ores_dq_calendar_events_artefact_tbl (id);
