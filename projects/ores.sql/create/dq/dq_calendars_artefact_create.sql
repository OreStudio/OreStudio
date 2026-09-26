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
-- Validated enumeration of named date collections consumed by ORE and QuantLib: business-day/holiday calendars (TARGET, UnitedStates, UnitedStates.GovernmentBond, ...), central-bank meeting calendars, and other calendar-shaped reference data. Each row is one concrete QuantLib/ORE calendar token — sub-market variants (e.g. UnitedStates.NYSE vs UnitedStates.GovernmentBond) are separate rows, not a joined variant field, so the code column always matches ORE's XML <Calendar> vocabulary verbatim. Classified by [[id:1A454661-81B5-4F8F-93A6-06547412DD84][calendar_type]] and associated with the [[id:88E8E1FB-6F2F-495F-BEC4-8C7ABEF68563][country]] whose calendar it is — supranational calendars (TARGET) use the ZZ sentinel (ISO 3166-1's own reserved user-assigned code) rather than a nullable country reference, since no single country owns them. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_calendars_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "calendar_type" text not null default 'public_holiday',
    "country_code" text not null default 'ZZ',
    "image_id" uuid null,
    "source" text not null default 'quantlib',
    "is_editable" boolean not null default false,
    "base_calendar_code" text null
);

create index if not exists dq_calendars_artefact_dataset_idx
on ores_dq_calendars_artefact_tbl (dataset_id);

create index if not exists dq_calendars_artefact_tenant_idx
on ores_dq_calendars_artefact_tbl (tenant_id);

create index if not exists dq_calendars_artefact_code_idx
on ores_dq_calendars_artefact_tbl (code);
