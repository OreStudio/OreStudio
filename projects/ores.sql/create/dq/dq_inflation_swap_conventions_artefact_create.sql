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
-- Describes how ORE builds an inflation swap: the calendars and conventions its legs roll on, the day count it accrues on, the zero-coupon inflation index it references, and how the index's observations are lagged and adjusted. Corresponds to the <InflationSwap> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). Ten fields are required and the rest are optional. The corpus sets all ten in all one hundred and one elements. Of the optional fields it sets only PublicationRoll, in two elements. The element also carries PublicationSchedule, which ORE types as a schedule of rule blocks, date blocks and derived schedule groups. Each block is written as one text column in a stated form: blocks are separated by semicolons and a block's fields by pipes, which is enough to keep every value the binding holds without a table of its own. Two shipped files set a schedule. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_inflation_swap_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "fix_calendar" text not null,
    "fix_convention" text not null,
    "day_count_fraction" text not null,
    "index" text not null,
    "interpolated" boolean not null,
    "observation_lag" text not null,
    "adjust_inflation_observation_dates" boolean not null,
    "inflation_calendar" text not null,
    "inflation_convention" text not null,
    "publication_roll" text null,
    "start_delay" text null,
    "start_delay_convention" text null,
    "publication_schedule_name" text null,
    "publication_schedule_rules" text null,
    "publication_schedule_dates" text null,
    "publication_schedule_derived_groups" text null,
    "oresmd_uri" text null
);

create index if not exists dq_inflation_swap_conventions_artefact_dataset_idx
on ores_dq_inflation_swap_conventions_artefact_tbl (dataset_id);

create index if not exists dq_inflation_swap_conventions_artefact_tenant_idx
on ores_dq_inflation_swap_conventions_artefact_tbl (tenant_id);

create index if not exists dq_inflation_swap_conventions_artefact_id_idx
on ores_dq_inflation_swap_conventions_artefact_tbl (id);
