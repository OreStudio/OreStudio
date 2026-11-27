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
-- Every store the installation keeps is a row here, before anything else knows about it. The row is what lets a person read "Calendar dates" instead of ores_refdata_calendar_dates_tbl, and what lets the sweep, the screen and the alerts agree on the set of stores without any of them carrying a list of tables. The catalogue is not a policy: it states what the store is and the bound it ships with, and a policy states what is applied now. That is why a store exists here whether or not a policy names it, and why the bound is one of three kinds rather than always an action and a period. A store bounded by its content keying needs no policy, and a store whose bound cannot be expressed as an age — the calendar days behind the horizon — states that it has none rather than carrying a default that would delete what pricing still needs. The rows describe the installation's own stores, so the platform authors them and the system tenant holds them: the classification that governs published rows makes the platform the author here, as it does for the policies those rows describe. The column is not null for that reason. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_retained_stores_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "display_name" text not null,
    "description" text not null,
    "owner_component" text not null,
    "scope" text not null,
    "target_kind" text not null,
    "target" text not null,
    "time_column" text null,
    "measure_kind" text not null,
    "bound_kind" text not null,
    "default_action" text null,
    "default_retention_days" integer null,
    "default_cadence_hours" integer not null,
    "default_reason" text null
);

create index if not exists dq_retained_stores_artefact_dataset_idx
on ores_dq_retained_stores_artefact_tbl (dataset_id);

create index if not exists dq_retained_stores_artefact_tenant_idx
on ores_dq_retained_stores_artefact_tbl (tenant_id);

create index if not exists dq_retained_stores_artefact_code_idx
on ores_dq_retained_stores_artefact_tbl (code);
