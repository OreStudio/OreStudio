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
-- The terms that state the futures contract itself, as ORE's ContractFrequency, Calendar, the expiry calendar, month lag and offset, the contract months, the business day convention, the averaging flag, the hours per day, the index name, the savings time, the delivery location, the balance of the month fields, the prohibited expiries and the future continuation mappings. It is a part because its fields hold one invariant: each describes the listed contract, and ORE requires the frequency and the calendar of every commodity future. So every convention of the kind fills this part once. The anchor day, the option expiry, the off-peak index and the averaging data beside it are optional, and each is a part of its own. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_commodity_future_contract_terms_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "convention_name" text not null,
    "kind_code" text not null,
    "contract_frequency" text not null,
    "calendar" text not null,
    "expiry_calendar" text null,
    "expiry_month_lag" integer null,
    "one_contract_month" text null,
    "offset_days" integer null,
    "business_day_convention" text null,
    "adjust_before_offset" boolean null,
    "is_averaging" boolean null,
    "valid_contract_months" text null,
    "hours_per_day" integer null,
    "index_name" text null,
    "savings_time" text null,
    "delivery_location" text null,
    "balance_of_the_month" boolean null,
    "balance_of_the_month_pricing_calendar" text null,
    "prohibited_expiries" text null,
    "future_continuation_mappings" text null
);

create index if not exists dq_commodity_future_contract_terms_artefact_dataset_idx
on ores_dq_commodity_future_contract_terms_artefact_tbl (dataset_id);

create index if not exists dq_commodity_future_contract_terms_artefact_tenant_idx
on ores_dq_commodity_future_contract_terms_artefact_tbl (tenant_id);

create index if not exists dq_commodity_future_contract_terms_artefact_convention_name_idx
on ores_dq_commodity_future_contract_terms_artefact_tbl (convention_name);
