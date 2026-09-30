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
 * Template: sql_asset_class_dq_artefact_populate.mustache
 *
 * Asset Class DQ Artefact Population Script
 *
 * Populates the three DQ staging tables with the asset-class taxonomy's badge
 * definitions, code domain and badge mappings. Generated from
 * ores.refdata.asset_class_catalogue; the system tenant's own rows are the
 * sibling dq_asset_class_system_populate.sql. Runs after the three artefact
 * mirrors, whose delete takes a whole dataset.
 *
 * This script is idempotent - replaces only its own rows.
 */

\echo '--- Asset Class DQ Artefacts ---'

select id as v_ac_def_ds from ores_dq_datasets_tbl
where code = 'ore.badge_definitions' and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_badge_definitions_artefact_tbl
where dataset_id = :'v_ac_def_ds' and code like 'asset_class\_%';

insert into ores_dq_badge_definitions_artefact_tbl (
    tenant_id, dataset_id, code, version, name, description, background_colour, text_colour, severity_code, css_class, display_order
)
values
    (ores_utility_system_tenant_id_fn(), :'v_ac_def_ds', 'asset_class_fx', 0, 'FX', 'Foreign exchange asset class.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 50),
    (ores_utility_system_tenant_id_fn(), :'v_ac_def_ds', 'asset_class_interest_rates', 0, 'Interest Rate', 'Interest rates asset class.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 51),
    (ores_utility_system_tenant_id_fn(), :'v_ac_def_ds', 'asset_class_credit', 0, 'Credit', 'Credit asset class.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 52),
    (ores_utility_system_tenant_id_fn(), :'v_ac_def_ds', 'asset_class_equity', 0, 'Equity', 'Equity asset class.', '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 53),
    (ores_utility_system_tenant_id_fn(), :'v_ac_def_ds', 'asset_class_commodity', 0, 'Commodity AC', 'Commodity asset class.', '#f97316', '#ffffff', 'warning', 'badge bg-warning', 54),
    (ores_utility_system_tenant_id_fn(), :'v_ac_def_ds', 'asset_class_inflation', 0, 'Inflation', 'Inflation asset class.', '#ec4899', '#ffffff', 'primary', 'badge bg-primary', 55),
    (ores_utility_system_tenant_id_fn(), :'v_ac_def_ds', 'asset_class_bond', 0, 'Bond', 'Bond asset class.', '#6366f1', '#ffffff', 'primary', 'badge bg-primary', 56)
;

select id as v_ac_dom_ds from ores_dq_datasets_tbl
where code = 'ore.code_domains' and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_code_domains_artefact_tbl
where dataset_id = :'v_ac_dom_ds' and code = '';

insert into ores_dq_code_domains_artefact_tbl (
    tenant_id, dataset_id, code, version, name, description, display_order
)
values
    (ores_utility_system_tenant_id_fn(), :'v_ac_dom_ds', 'asset_class', 0, 'Asset Class', 'Top-level product classification codes (fx, interest_rates, credit, equity, commodity, inflation, bond), shown on instrument_code and asset_class_code.', 30)
;

select id as v_ac_map_ds from ores_dq_datasets_tbl
where code = 'ore.badge_mappings' and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_badge_mappings_artefact_tbl
where dataset_id = :'v_ac_map_ds' and code_domain_code = '';

insert into ores_dq_badge_mappings_artefact_tbl (
    tenant_id, dataset_id, code_domain_code, entity_code, badge_code, version
)
values
    (ores_utility_system_tenant_id_fn(), :'v_ac_map_ds', 'asset_class', 'fx', 'asset_class_fx', 0),
    (ores_utility_system_tenant_id_fn(), :'v_ac_map_ds', 'asset_class', 'interest_rates', 'asset_class_interest_rates', 0),
    (ores_utility_system_tenant_id_fn(), :'v_ac_map_ds', 'asset_class', 'credit', 'asset_class_credit', 0),
    (ores_utility_system_tenant_id_fn(), :'v_ac_map_ds', 'asset_class', 'equity', 'asset_class_equity', 0),
    (ores_utility_system_tenant_id_fn(), :'v_ac_map_ds', 'asset_class', 'commodity', 'asset_class_commodity', 0),
    (ores_utility_system_tenant_id_fn(), :'v_ac_map_ds', 'asset_class', 'inflation', 'asset_class_inflation', 0),
    (ores_utility_system_tenant_id_fn(), :'v_ac_map_ds', 'asset_class', 'bond', 'asset_class_bond', 0)
;
