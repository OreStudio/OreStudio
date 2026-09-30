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

/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_asset_class_dq_system_populate.mustache
 *
 * Asset Class DQ Rows Population Script
 *
 * Seeds the system tenant's badge definitions, the asset_class code domain and
 * the badge mappings for the refdata product taxonomy. Generated from
 * ores.refdata.asset_class_catalogue; the staging mirrors are the sibling
 * dq_asset_class_artefact_populate.sql.
 *
 * This script is idempotent (uses upsert functions).
 */

\echo '--- Asset Class DQ Rows ---'

do $$
begin
    perform ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class_fx', 'FX', 'Foreign exchange asset class.',
        '#3b82f6', '#ffffff', 'info', 'badge bg-info', 50);
    perform ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class_interest_rates', 'Interest Rate', 'Interest rates asset class.',
        '#14b8a6', '#ffffff', 'info', 'badge bg-info', 51);
    perform ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class_credit', 'Credit', 'Credit asset class.',
        '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 52);
    perform ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class_equity', 'Equity', 'Equity asset class.',
        '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 53);
    perform ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class_commodity', 'Commodity AC', 'Commodity asset class.',
        '#f97316', '#ffffff', 'warning', 'badge bg-warning', 54);
    perform ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class_inflation', 'Inflation', 'Inflation asset class.',
        '#ec4899', '#ffffff', 'primary', 'badge bg-primary', 55);
    perform ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class_bond', 'Bond', 'Bond asset class.',
        '#6366f1', '#ffffff', 'primary', 'badge bg-primary', 56);
    perform ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class', 'Asset Class',
        'Top-level product classification codes (fx, interest_rates, credit, equity, commodity, inflation, bond), shown on instrument_code and asset_class_code.', 30);
    perform ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class', 'fx', 'asset_class_fx');
    perform ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class', 'interest_rates', 'asset_class_interest_rates');
    perform ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class', 'credit', 'asset_class_credit');
    perform ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class', 'equity', 'asset_class_equity');
    perform ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class', 'commodity', 'asset_class_commodity');
    perform ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class', 'inflation', 'asset_class_inflation');
    perform ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'asset_class', 'bond', 'asset_class_bond');
end $$;
