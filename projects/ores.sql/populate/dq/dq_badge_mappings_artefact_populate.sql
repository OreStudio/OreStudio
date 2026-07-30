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
 * Badge Mappings Artefact Population Script
 *
 * Populates the dq_badge_mappings_artefact_tbl staging table. Mirrors the
 * same 109 mappings seeded directly into ores_dq_badge_mappings_tbl (for
 * the system tenant itself) by dq_badge_system_populate.sql -- kept in sync
 * by hand (extracted programmatically from that file when first written);
 * this script is what makes those rows publishable to other tenants.
 *
 * To publish to a tenant:
 *   SELECT * FROM ores_dq_badge_mappings_publish_fn(
 *       (SELECT id FROM ores_dq_datasets_tbl WHERE code = 'ore.badge_mappings' AND valid_to = ores_utility_infinity_timestamp_fn()),
 *       <target_tenant_id>,
 *       'upsert'
 *   );
 */

\echo '--- Badge Mappings Artefacts ---'

select id as v_dataset_id from ores_dq_datasets_tbl where code = 'ore.badge_mappings' and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_badge_mappings_artefact_tbl
where dataset_id = :'v_dataset_id';

insert into ores_dq_badge_mappings_artefact_tbl (
    tenant_id, dataset_id, code_domain_code, entity_code, badge_code, version
)
values
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_status', 'Active', 'active', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'calendar_type', 'public_holiday', 'calendar_type_public_holiday', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'calendar_type', 'central_bank_meeting', 'calendar_type_central_bank_meeting', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'calendar_type', 'financial_centre', 'calendar_type_financial_centre', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'calendar_type', 'data_release', 'calendar_type_data_release', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'calendar_type', 'other', 'calendar_type_other', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_status', 'Inactive', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_status', 'Suspended', 'frozen', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_type', 'Internal', 'account_type_service', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_type', 'Bank', 'classification_major', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_type', 'CorporateGroup', 'origin_primary', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_type', 'HedgeFund', 'classification_exotic', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_type', 'Corporate', 'classification_minor', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_type', 'CentralBank', 'treatment_enriched', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_type', 'Exchange', 'state_running', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_type', 'Individual', 'nature_simulated', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_status', 'Active', 'active', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_status', 'Closed', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_status', 'Frozen', 'frozen', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'regulatory_book_type', 'Trading', 'pending', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'regulatory_book_type', 'Banking', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'is_sweepable', 'true', 'active', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'is_sweepable', 'false', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'portfolio_status', 'Active', 'active', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'portfolio_status', 'Inactive', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'portfolio_status', 'Closed', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'portfolio_status', 'Frozen', 'frozen', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'portfolio_status', 'Pending', 'pending', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'portfolio_type', 'Virtual', 'type_virtual', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'portfolio_type', 'Physical', 'type_physical', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'is_virtual', 'true', 'active', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'is_virtual', 'false', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_type', 'Trading', 'type_virtual', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_type', 'Banking', 'type_physical', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'login_status', 'Online', 'login_online', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'login_status', 'Recent', 'login_recent', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'login_status', 'Old', 'login_old', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'login_status', 'Never', 'login_never', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_locked', 'Locked', 'account_locked', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_locked', 'Unlocked', 'account_unlocked', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_online', 'Yes', 'login_online', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_online', 'No', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_classification', 'major', 'classification_major', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_classification', 'minor', 'classification_minor', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_classification', 'exotic', 'classification_exotic', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_classification', 'commodity', 'classification_commodity', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_business_day_convention', 'Following', 'active', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_business_day_convention', 'ModifiedFollowing', 'origin_primary', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_business_day_convention', 'Preceding', 'nature_simulated', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_business_day_convention', 'ModifiedPreceding', 'account_type_service', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_business_day_convention', 'Unadjusted', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_business_day_convention', 'HalfMonthModifiedFollowing', 'tenant_bootstrapping', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_business_day_convention', 'Nearest', 'treatment_enriched', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_spot_relative', 'Yes', 'login_online', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_spot_relative', 'No', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_end_of_month', 'Yes', 'login_online', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_end_of_month', 'No', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute_task_outcome', 'Success', 'outcome_success', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute_task_outcome', 'Failed', 'outcome_failed', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute_task_outcome', 'No Reply', 'outcome_no_reply', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute_task_outcome', 'Pending', 'pending', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute_task_state', 'Running', 'state_running', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute_task_state', 'Unsent', 'pending', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute_task_state', 'Done', 'state_done', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute_task_state', 'Inactive', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_concurrency_policy', 'Skip', 'policy_skip', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_concurrency_policy', 'Queue', 'policy_queue', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_concurrency_policy', 'Fail', 'policy_fail', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_fsm_state', 'Active', 'active', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_fsm_state', 'Draft', 'fsm_draft', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_fsm_state', 'Suspended', 'fsm_suspended', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_fsm_state', 'Archived', 'archived', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_origin', 'Primary', 'origin_primary', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_origin', 'Derived', 'origin_derived', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_nature', 'Actual', 'nature_actual', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_nature', 'Estimated', 'nature_estimated', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_nature', 'Simulated', 'nature_simulated', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'workspace_status', 'active', 'active', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'workspace_status', 'archived', 'archived', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenant_status', 'bootstrapping', 'tenant_bootstrapping', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenant_status', 'active', 'active', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenant_status', 'suspended', 'frozen', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenant_status', 'terminated', 'archived', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_treatment', 'Raw', 'treatment_raw', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_treatment', 'Cleaned', 'treatment_cleaned', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_treatment', 'Enriched', 'treatment_enriched', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_type', 'user', 'account_type_user', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_type', 'service', 'account_type_service', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_type', 'algorithm', 'account_type_algorithm', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_type', 'llm', 'account_type_llm', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'monetary_nature', 'fiat', 'nature_fiat', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'monetary_nature', 'commodity', 'nature_commodity_currency', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'monetary_nature', 'synthetic', 'nature_synthetic', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'monetary_nature', 'supranational', 'nature_supranational', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_market_tier', 'g10', 'tier_g10', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_market_tier', 'emerging', 'tier_emerging', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_market_tier', 'exotic', 'tier_exotic', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_market_tier', 'frontier', 'tier_frontier', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_market_tier', 'historical', 'tier_historical', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_kind', 'PERIOD', 'type_physical', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_kind', 'SPECIAL', 'type_virtual', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_unit', 'DAY', 'origin_primary', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_unit', 'WEEK', 'nature_simulated', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_unit', 'MONTH', 'treatment_enriched', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_unit', 'YEAR', 'state_running', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_unit', 'NONE', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'crm_enabled', 'true', 'active', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'crm_enabled', 'false', 'inactive', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class', 'fx', 'asset_class_fx', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class', 'rates', 'asset_class_rates', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class', 'credit', 'asset_class_credit', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class', 'equity', 'asset_class_equity', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class', 'commodity', 'asset_class_commodity', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class', 'inflation', 'asset_class_inflation', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class', 'bond', 'asset_class_bond', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class', 'cross_asset', 'asset_class_cross_asset', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type', 'Funding', 'book_purpose_type_funding', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type', 'RemittanceTarget', 'book_purpose_type_remittance_target', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type', 'Reserve', 'book_purpose_type_reserve', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type', 'Sales', 'book_purpose_type_sales', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type', 'SweepTarget', 'book_purpose_type_sweep_target', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type', 'Test', 'book_purpose_type_test', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type', 'Trading', 'book_purpose_type_trading', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type', 'Wash', 'book_purpose_type_wash', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_purpose_type', 'WriteOff', 'book_purpose_type_write_off', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'business_unit_type', 'BRANCH', 'business_unit_type_branch', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'business_unit_type', 'BUSINESS_AREA', 'business_unit_type_business_area', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'business_unit_type', 'COST_CENTRE', 'business_unit_type_cost_centre', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'business_unit_type', 'DESK', 'business_unit_type_desk', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'business_unit_type', 'DIVISION', 'business_unit_type_division', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'contact_type', 'Billing', 'contact_type_billing', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'contact_type', 'Legal', 'contact_type_legal', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'contact_type', 'Operations', 'contact_type_operations', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'contact_type', 'Settlement', 'contact_type_settlement', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'ledger_feed_type', 'Automatic', 'ledger_feed_type_automatic', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'ledger_feed_type', 'Manual', 'ledger_feed_type_manual', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'ledger_feed_type', 'None', 'ledger_feed_type_none', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type', 'AssetSwap', 'leg_type_asset_swap', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type', 'CMS', 'leg_type_cms', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type', 'CMSSpread', 'leg_type_cms_spread', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type', 'CPI', 'leg_type_cpi', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type', 'DigitalCMS', 'leg_type_digital_cms', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type', 'Fixed', 'leg_type_fixed', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type', 'Floating', 'leg_type_floating', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type', 'OIS', 'leg_type_ois', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type', 'YoY', 'leg_type_yoy', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'leg_type', 'ZeroCoupon', 'leg_type_zero_coupon', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'purpose_type', 'ClientReporting', 'purpose_type_client_reporting', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'purpose_type', 'Internal', 'purpose_type_internal', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'purpose_type', 'Regulatory', 'purpose_type_regulatory', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'purpose_type', 'Risk', 'purpose_type_risk', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'rounding_type', 'Ceiling', 'rounding_type_ceiling', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'rounding_type', 'Closest', 'rounding_type_closest', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'rounding_type', 'Down', 'rounding_type_down', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'rounding_type', 'Floor', 'rounding_type_floor', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'rounding_type', 'Up', 'rounding_type_up', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor', 'IMM_ROLL', 'tenor_anchor_imm_roll', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor', 'NEAR_LEG', 'tenor_anchor_near_leg', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor', 'NONE', 'tenor_anchor_none', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor', 'SPOT', 'tenor_anchor_spot', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor', 'TODAY', 'tenor_anchor_today', 0),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_anchor', 'TOMORROW', 'tenor_anchor_tomorrow', 0);
