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
-- The day the expiry of a commodity future is anchored to, as ORE's AnchorDay: a day of the month, a number of calendar days before the month end, a number of business days after the month start, the nth weekday, the last weekday, or a weekly day. It is a part because its fields hold one invariant: together they state one anchor, and ORE allows one form of it at a time. The element is optional, so a convention fills this part at most once. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_commodity_future_expiry_anchors_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "convention_name" text not null,
    "kind_code" text not null,
    "anchor_day_of_month" integer null,
    "anchor_calendar_days_before" integer null,
    "anchor_business_days_after" integer null,
    "anchor_nth_nth" integer null,
    "anchor_nth_weekday" text null,
    "anchor_last_weekday" text null,
    "anchor_weekly_day_of_the_week" text null
);

create index if not exists dq_commodity_future_expiry_anchors_artefact_dataset_idx
on ores_dq_commodity_future_expiry_anchors_artefact_tbl (dataset_id);

create index if not exists dq_commodity_future_expiry_anchors_artefact_tenant_idx
on ores_dq_commodity_future_expiry_anchors_artefact_tbl (tenant_id);

create index if not exists dq_commodity_future_expiry_anchors_artefact_convention_name_idx
on ores_dq_commodity_future_expiry_anchors_artefact_tbl (convention_name);
