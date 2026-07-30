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
 * Badge Definitions Artefact Population Script
 *
 * Populates the dq_badge_definitions_artefact_tbl staging table. Mirrors the
 * same 57 definitions seeded directly into ores_dq_badge_definitions_tbl
 * (for the system tenant itself) by dq_badge_system_populate.sql -- kept in
 * sync by hand (extracted programmatically from that file when first
 * written); this script is what makes those rows publishable to other
 * tenants.
 *
 * To publish to a tenant:
 *   SELECT * FROM ores_dq_badge_definitions_publish_fn(
 *       (SELECT id FROM ores_dq_datasets_tbl WHERE code = 'ore.badge_definitions' AND valid_to = ores_utility_infinity_timestamp_fn()),
 *       <target_tenant_id>,
 *       'upsert'
 *   );
 */

\echo '--- Badge Definitions Artefacts ---'

select id as v_dataset_id from ores_dq_datasets_tbl where code = 'ore.badge_definitions' and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_badge_definitions_artefact_tbl
where dataset_id = :'v_dataset_id';

insert into ores_dq_badge_definitions_artefact_tbl (
    tenant_id, dataset_id, code, version, name, description, background_colour, text_colour, severity_code, css_class, display_order
)
values
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'active', 0, 'Active', 'Record is active and operational.', '#22c55e', '#ffffff', 'success', 'badge bg-success', 1),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'inactive', 0, 'Inactive', 'Record is inactive or not operational.', '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 2),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'frozen', 0, 'Frozen', 'Record is frozen; no changes permitted.', '#eab308', '#ffffff', 'warning', 'badge bg-warning', 3),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'pending', 0, 'Pending', 'Record is awaiting processing or approval.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 4),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'type_virtual', 0, 'Virtual', 'Entity is a virtual construct.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 5),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'type_physical', 0, 'Physical', 'Entity is a physical construct.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 6),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'login_online', 0, 'Online', 'User is currently logged in.', '#22c55e', '#ffffff', 'success', 'badge bg-success', 7),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'login_recent', 0, 'Recent', 'User logged in recently.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 8),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'login_old', 0, 'Old', 'User has not logged in for a long time.', '#eab308', '#ffffff', 'warning', 'badge bg-warning', 9),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'login_never', 0, 'Never', 'User has never logged in.', '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 10),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_locked', 0, 'Locked', 'Account is locked and cannot be used.', '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 11),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_unlocked', 0, 'Unlocked', 'Account is unlocked and accessible.', '#22c55e', '#ffffff', 'success', 'badge bg-success', 12),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'outcome_success', 0, 'Success', 'Task completed successfully.', '#22c55e', '#ffffff', 'success', 'badge bg-success', 13),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'outcome_failed', 0, 'Failed', 'Task failed to complete.', '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 14),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'outcome_no_reply', 0, 'No Reply', 'Task received no reply from worker.', '#eab308', '#ffffff', 'warning', 'badge bg-warning', 15),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'state_running', 0, 'Running', 'Task is currently executing.', '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 16),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'state_done', 0, 'Done', 'Task has completed.', '#22c55e', '#ffffff', 'success', 'badge bg-success', 17),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'policy_skip', 0, 'Skip', 'Skip overlapping executions.', '#eab308', '#ffffff', 'warning', 'badge bg-warning', 18),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'policy_queue', 0, 'Queue', 'Queue overlapping executions.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 19),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'policy_fail', 0, 'Fail', 'Fail on overlapping executions.', '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 20),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'fsm_draft', 0, 'Draft', 'Report definition is in draft state.', '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 21),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'fsm_suspended', 0, 'Suspended', 'Report definition is suspended.', '#eab308', '#ffffff', 'warning', 'badge bg-warning', 22),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'archived', 0, 'Archived', 'Record has been archived and is no longer active.', '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 23),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'origin_primary', 0, 'Primary', 'Data originates from a primary source.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 24),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'origin_derived', 0, 'Derived', 'Data is derived from another source.', '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 25),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'nature_actual', 0, 'Actual', 'Real-world data.', '#22c55e', '#ffffff', 'success', 'badge bg-success', 26),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'nature_estimated', 0, 'Estimated', 'Data is an estimation.', '#eab308', '#ffffff', 'warning', 'badge bg-warning', 27),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'nature_simulated', 0, 'Simulated', 'Data is simulated.', '#ec4899', '#ffffff', 'primary', 'badge bg-primary', 28),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'treatment_raw', 0, 'Raw', 'Data has not been processed.', '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 29),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'treatment_cleaned', 0, 'Cleaned', 'Data has been cleaned.', '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 30),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'treatment_enriched', 0, 'Enriched', 'Data has been enriched with additional context.', '#a855f7', '#ffffff', 'primary', 'badge bg-primary', 31),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenant_bootstrapping', 0, 'Bootstrapping', 'Tenant is awaiting initial provisioning wizard run.', '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 32),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_type_user', 0, 'User', 'Human user account.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 33),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_type_service', 0, 'Service', 'Service account for non-human processes.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 34),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_type_algorithm', 0, 'Algorithm', 'Account for automated algorithms.', '#eab308', '#ffffff', 'warning', 'badge bg-warning', 35),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_type_llm', 0, 'LLM', 'Account for Large Language Model agents.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 36),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'classification_major', 0, 'Major', 'Major currency pair (most liquid tier).', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 37),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'classification_minor', 0, 'Minor', 'Minor currency pair (mid liquidity tier).', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 38),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'classification_exotic', 0, 'Exotic', 'Exotic currency pair (least liquid tier).', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 39),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'classification_commodity', 0, 'Commodity', 'Commodity-linked currency pair.', '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 40),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'nature_fiat', 0, 'Fiat', 'Government-issued currency not backed by a commodity.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 41),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'nature_commodity_currency', 0, 'Commodity Currency', 'Currency backed by or representing a physical commodity.', '#eab308', '#ffffff', 'warning', 'badge bg-warning', 42),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'nature_synthetic', 0, 'Synthetic', 'Artificially constructed currency or index.', '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 43),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'nature_supranational', 0, 'Supranational', 'Currency issued by a multi-national authority.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 44),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'calendar_type_public_holiday', 0, 'Public Holiday', 'A business-day/holiday calendar for a country, currency, or supranational region.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 60),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'calendar_type_central_bank_meeting', 0, 'Central Bank Meeting', 'A calendar of a central bank''s scheduled policy-meeting dates.', '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 61),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'calendar_type_financial_centre', 0, 'Financial Centre', 'A business-day calendar for a specific financial centre or exchange.', '#22c55e', '#ffffff', 'success', 'badge bg-success', 62),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'calendar_type_data_release', 0, 'Data Release', 'A calendar of scheduled macroeconomic data release dates.', '#eab308', '#ffffff', 'warning', 'badge bg-warning', 63),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'calendar_type_other', 0, 'Other', 'A calendar not classified under any other calendar type.', '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 64),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tier_g10', 0, 'G10', 'Major currency with deep liquidity and tight spreads.', '#22c55e', '#ffffff', 'success', 'badge bg-success', 45),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tier_emerging', 0, 'Emerging', 'Currency from a developing economy with moderate liquidity.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 46),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tier_exotic', 0, 'Exotic Tier', 'Thinly traded currency with wide spreads.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 47),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tier_frontier', 0, 'Frontier', 'Currency from a frontier market with limited convertibility.', '#eab308', '#ffffff', 'warning', 'badge bg-warning', 48),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tier_historical', 0, 'Historical', 'Currency no longer in active use.', '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 49),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class_fx', 0, 'FX', 'Foreign exchange asset class.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 50),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class_rates', 0, 'Rates', 'Interest rates asset class.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 51),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class_credit', 0, 'Credit', 'Credit asset class.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 52),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class_equity', 0, 'Equity', 'Equity asset class.', '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 53),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class_commodity', 0, 'Commodity AC', 'Commodity asset class.', '#f97316', '#ffffff', 'warning', 'badge bg-warning', 54),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class_inflation', 0, 'Inflation', 'Inflation asset class.', '#ec4899', '#ffffff', 'primary', 'badge bg-primary', 55),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class_bond', 0, 'Bond', 'Bond asset class.', '#6366f1', '#ffffff', 'primary', 'badge bg-primary', 56),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class_cross_asset', 0, 'Cross Asset', 'Cross-asset asset class.', '#64748b', '#ffffff', 'secondary', 'badge bg-secondary', 57),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type_funding', 0, 'Funding', 'Book used for funding purposes.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 65),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type_remittance_target', 0, 'Remittance Target', 'Book that receives remitted balances.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 66),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type_reserve', 0, 'Reserve', 'Book holding reserve balances.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 67),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type_sales', 0, 'Sales', 'Book used for client sales activity.', '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 68),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type_sweep_target', 0, 'Sweep Target', 'Book that receives spot-swept balances.', '#a855f7', '#ffffff', 'primary', 'badge bg-primary', 69),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type_test', 0, 'Test', 'Book used for testing, not production activity.', '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 70),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type_trading', 0, 'Trading', 'Book used for trading activity.', '#ec4899', '#ffffff', 'primary', 'badge bg-primary', 71),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type_wash', 0, 'Wash', 'Book used for wash/internal offsetting trades.', '#6366f1', '#ffffff', 'primary', 'badge bg-primary', 72),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type_write_off', 0, 'Write-off', 'Book used to write off balances.', '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 73),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'business_unit_type_branch', 0, 'Branch', 'A geographic branch business unit.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 74),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'business_unit_type_business_area', 0, 'Business Area', 'A broad business-area business unit.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 75),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'business_unit_type_cost_centre', 0, 'Cost Centre', 'A cost-centre business unit.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 76),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'business_unit_type_desk', 0, 'Desk', 'A trading-desk business unit.', '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 77),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'business_unit_type_division', 0, 'Division', 'A divisional business unit.', '#a855f7', '#ffffff', 'primary', 'badge bg-primary', 78),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'contact_type_billing', 0, 'Billing', 'A billing contact.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 79),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'contact_type_legal', 0, 'Legal', 'A legal contact.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 80),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'contact_type_operations', 0, 'Operations', 'An operations contact.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 81),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'contact_type_settlement', 0, 'Settlement', 'A settlement contact.', '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 82),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'ledger_feed_type_automatic', 0, 'Automatic', 'Ledger is fed automatically.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 83),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'ledger_feed_type_manual', 0, 'Manual', 'Ledger is fed manually.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 84),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'ledger_feed_type_none', 0, 'No Feed', 'Ledger has no feed configured.', '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 85),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type_asset_swap', 0, 'Asset Swap', 'An asset swap trade leg.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 86),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type_cms', 0, 'CMS', 'A constant maturity swap trade leg.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 87),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type_cms_spread', 0, 'CMS Spread', 'A CMS spread trade leg.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 88),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type_cpi', 0, 'CPI', 'A CPI-linked trade leg.', '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 89),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type_digital_cms', 0, 'Digital CMS', 'A digital CMS trade leg.', '#a855f7', '#ffffff', 'primary', 'badge bg-primary', 90),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type_fixed', 0, 'Fixed', 'A fixed-rate trade leg.', '#ec4899', '#ffffff', 'primary', 'badge bg-primary', 91),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type_floating', 0, 'Floating', 'A floating-rate trade leg.', '#6366f1', '#ffffff', 'primary', 'badge bg-primary', 92),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type_ois', 0, 'OIS', 'An overnight index swap trade leg.', '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 93),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type_yoy', 0, 'YoY', 'A year-on-year inflation trade leg.', '#64748b', '#ffffff', 'secondary', 'badge bg-secondary', 94),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type_zero_coupon', 0, 'Zero Coupon', 'A zero coupon trade leg.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 95),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'purpose_type_client_reporting', 0, 'Client Reporting', 'Portfolio used for client-facing reporting.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 96),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'purpose_type_internal', 0, 'Internal', 'Portfolio used for internal purposes.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 97),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'purpose_type_regulatory', 0, 'Regulatory', 'Portfolio used for regulatory capital and compliance reporting.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 98),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'purpose_type_risk', 0, 'Risk', 'Portfolio used for risk management purposes.', '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 99),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'rounding_type_ceiling', 0, 'Ceiling', 'Always rounds up to the next increment.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 100),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'rounding_type_closest', 0, 'Closest', 'Rounds to the nearest increment.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 101),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'rounding_type_down', 0, 'Down', 'Always rounds down to the previous increment.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 102),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'rounding_type_floor', 0, 'Floor', 'Always rounds down to the floor increment.', '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 103),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'rounding_type_up', 0, 'Up', 'Always rounds up to the next increment.', '#a855f7', '#ffffff', 'primary', 'badge bg-primary', 104),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor_imm_roll', 0, 'IMM Roll', 'Tenor anchored to an IMM roll date.', '#3b82f6', '#ffffff', 'info', 'badge bg-info', 105),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor_near_leg', 0, 'Near Leg', 'Tenor anchored to the near leg of an FX swap.', '#14b8a6', '#ffffff', 'info', 'badge bg-info', 106),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor_none', 0, 'No Anchor', 'Tenor has no special anchor.', '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 107),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor_spot', 0, 'Spot', 'Tenor anchored to the spot date.', '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 108),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor_today', 0, 'Today', 'Tenor anchored to today''s date.', '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 109),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor_tomorrow', 0, 'Tomorrow', 'Tenor anchored to tomorrow''s date.', '#a855f7', '#ffffff', 'primary', 'badge bg-primary', 110);
