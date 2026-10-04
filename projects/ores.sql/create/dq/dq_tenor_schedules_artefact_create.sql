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
-- A named schedule axis a [[id:9A2E4D6B-7C1F-4B8A-A5D3-2F6E9B1C4A87][tenor]] resolves along (story Decision D2: anchor + calendar offset + n steps). Two kinds today, distinguished by schedule_source: - CLOSED_FORM: the dates come from a closed-form rule evaluated code-side. ROLL_QUARTER is the only instance: the first business day after the 20th of March/June/September/December (the IMM quarterly rule). - EVENT_LOOKUP: the dates come from [[id:B20050A5-1245-4944-A328-2A0893C92AEC][calendar_event]] rows on a named calendar, filtered by diary entry type. FOMC_MEETING is the only instance: central_bank_meeting events on US.FOMC. calendar_code and diary_entry_type are null for closed-form schedules (no event store involved) and required for event-lookup ones -- but the binding is documented, not enforced in the schema. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_tenor_schedules_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "display_order" integer not null default 0,
    "schedule_source" text not null default 'CLOSED_FORM',
    "calendar_code" text null,
    "diary_entry_type" text null
);

create index if not exists dq_tenor_schedules_artefact_dataset_idx
on ores_dq_tenor_schedules_artefact_tbl (dataset_id);

create index if not exists dq_tenor_schedules_artefact_tenant_idx
on ores_dq_tenor_schedules_artefact_tbl (tenant_id);

create index if not exists dq_tenor_schedules_artefact_code_idx
on ores_dq_tenor_schedules_artefact_tbl (code);
