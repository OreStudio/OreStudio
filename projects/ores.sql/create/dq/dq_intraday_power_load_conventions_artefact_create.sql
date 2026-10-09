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
-- Describes how ORE builds an intraday power load profile, either as a list of dates each carrying its own load factors or as a list of business day rules that generate them. Corresponds to the <IntradayPowerLoad> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). The element's one data field is a choice between two nested lists, and each list is written as one text column in a stated form rather than given a table of its own. Records are separated by semicolons and a record's fields by pipes; load factors are separated by commas and a factor's own fields by colons, so a factor reads from:to:unit:dst:value. One file carries two elements, one of each form, and between them they hold every shape the element can take. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_intraday_power_load_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "explicit_load_profile" text null,
    "business_day_load_rules" text null,
    "oresmd_uri" text null
);

create index if not exists dq_intraday_power_load_conventions_artefact_dataset_idx
on ores_dq_intraday_power_load_conventions_artefact_tbl (dataset_id);

create index if not exists dq_intraday_power_load_conventions_artefact_tenant_idx
on ores_dq_intraday_power_load_conventions_artefact_tbl (tenant_id);

create index if not exists dq_intraday_power_load_conventions_artefact_id_idx
on ores_dq_intraday_power_load_conventions_artefact_tbl (id);
