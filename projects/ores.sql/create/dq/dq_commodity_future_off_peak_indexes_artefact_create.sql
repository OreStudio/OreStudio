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
-- The peak and off-peak indices a power future's price is made of, as ORE's OffPeakPowerIndexData: the off-peak index, the peak index, the hours of the day that count as off-peak, and the calendar the peak hours are read on. It is a part because its fields hold one invariant: ORE requires all four together, so the part makes each column required. Only a power future states it, so a convention fills this part at most once. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_commodity_future_off_peak_indexes_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "convention_name" text not null,
    "kind_code" text not null,
    "off_peak_index" text not null,
    "peak_index" text not null,
    "off_peak_hours" double precision not null,
    "peak_calendar" text not null
);

create index if not exists dq_commodity_future_off_peak_indexes_artefact_dataset_idx
on ores_dq_commodity_future_off_peak_indexes_artefact_tbl (dataset_id);

create index if not exists dq_commodity_future_off_peak_indexes_artefact_tenant_idx
on ores_dq_commodity_future_off_peak_indexes_artefact_tbl (tenant_id);

create index if not exists dq_commodity_future_off_peak_indexes_artefact_convention_name_idx
on ores_dq_commodity_future_off_peak_indexes_artefact_tbl (convention_name);
