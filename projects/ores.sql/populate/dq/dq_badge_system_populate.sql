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
 * Badge System Population Script
 *
 * Seeds the database with badge severities, code domains, badge definitions,
 * and badge mappings. This is the single source of truth for all badge
 * visual metadata, migrated from the hardcoded values in BadgeColors.hpp
 * and ColorConstants.hpp.
 *
 * This script is idempotent (uses upsert functions).
 *
 * Severity codes align with Bootstrap 5 contextual classes.
 */

DO $$
BEGIN
    -- =============================================================================
    -- Badge Severities
    -- =============================================================================

    -- --- Badge Severities ---

    PERFORM ores_dq_badge_severities_upsert_fn(ores_utility_system_tenant_id_fn(),
        'secondary', 'Secondary', 'Muted or neutral state. No special attention required.', 1);

    PERFORM ores_dq_badge_severities_upsert_fn(ores_utility_system_tenant_id_fn(),
        'info', 'Info', 'Informational state. General context, no action needed.', 2);

    PERFORM ores_dq_badge_severities_upsert_fn(ores_utility_system_tenant_id_fn(),
        'success', 'Success', 'Positive or active state. Everything is in order.', 3);

    PERFORM ores_dq_badge_severities_upsert_fn(ores_utility_system_tenant_id_fn(),
        'warning', 'Warning', 'Caution state. Attention may be required.', 4);

    PERFORM ores_dq_badge_severities_upsert_fn(ores_utility_system_tenant_id_fn(),
        'danger', 'Danger', 'Error or critical state. Immediate attention required.', 5);

    PERFORM ores_dq_badge_severities_upsert_fn(ores_utility_system_tenant_id_fn(),
        'primary', 'Primary', 'Highlighted or primary action state.', 6);

    -- =============================================================================
    -- Code Domains
    -- =============================================================================

    -- --- Code Domains ---

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'party_status', 'Party Status',
        'Lifecycle status codes for party and counterparty records.', 1);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'book_status', 'Book Status',
        'Lifecycle status codes for book records.', 2);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'regulatory_book_type', 'Regulatory Book Type',
        'Basel III/IV FRTB trading book / banking book classification a book carries.', 19);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'is_sweepable', 'Sweepable',
        'Spot-sweep eligibility flag for a book (Yes, No).', 27);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'portfolio_status', 'Portfolio Status',
        'Lifecycle status codes for portfolio records.', 3);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'portfolio_type', 'Portfolio Type',
        'Type codes for portfolio records (Virtual, Physical).', 4);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'book_type', 'Book Type',
        'Type codes for book records (Trading, Banking).', 5);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'login_status', 'Login Status',
        'Login activity status for user accounts (Never, Old, Recent, Online).', 6);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_locked', 'Account Locked',
        'Account lock status (Locked, Unlocked).', 7);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'compute_task_state', 'Compute Task State',
        'State codes for compute task records.', 8);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'compute_task_outcome', 'Compute Task Outcome',
        'Outcome codes for completed compute tasks.', 9);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'report_concurrency_policy', 'Report Concurrency Policy',
        'Concurrency policy codes for report definitions (Skip, Queue, Fail).', 10);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'report_fsm_state', 'Report FSM State',
        'FSM lifecycle state codes for report definitions.', 11);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenant_status', 'Tenant Status',
        'Lifecycle status codes for tenant records (bootstrapping, active, suspended, terminated).', 15);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_origin', 'DQ Origin',
        'Data quality origin dimension codes.', 12);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_nature', 'DQ Nature',
        'Data quality nature dimension codes.', 13);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_treatment', 'DQ Treatment',
        'Data quality treatment dimension codes.', 14);

    -- =============================================================================
    -- Badge Definitions
    -- Colours migrated from ColorConstants.hpp and BadgeColors.hpp.
    -- text_colour is always #ffffff (white) for all current badges.
    -- =============================================================================

    -- --- Badge Definitions ---

    -- Status: Active
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'active', 'Active', 'Record is active and operational.',
        '#22c55e', '#ffffff', 'success', 'badge bg-success', 1);

    -- Status: Inactive / Closed
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'inactive', 'Inactive', 'Record is inactive or not operational.',
        '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 2);

    -- Status: Frozen
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'frozen', 'Frozen', 'Record is frozen; no changes permitted.',
        '#eab308', '#ffffff', 'warning', 'badge bg-warning', 3);

    -- Status: Pending
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'pending', 'Pending', 'Record is awaiting processing or approval.',
        '#3b82f6', '#ffffff', 'info', 'badge bg-info', 4);

    -- Entity type: Virtual / Portfolio
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'type_virtual', 'Virtual', 'Entity is a virtual construct.',
        '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 5);

    -- Entity type: Physical / Trading
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'type_physical', 'Physical', 'Entity is a physical construct.',
        '#3b82f6', '#ffffff', 'info', 'badge bg-info', 6);

    -- Login status: Online
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'login_online', 'Online', 'User is currently logged in.',
        '#22c55e', '#ffffff', 'success', 'badge bg-success', 7);

    -- Login status: Recent
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'login_recent', 'Recent', 'User logged in recently.',
        '#3b82f6', '#ffffff', 'info', 'badge bg-info', 8);

    -- Login status: Old
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'login_old', 'Old', 'User has not logged in for a long time.',
        '#eab308', '#ffffff', 'warning', 'badge bg-warning', 9);

    -- Login status: Never
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'login_never', 'Never', 'User has never logged in.',
        '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 10);

    -- Account: Locked
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_locked', 'Locked', 'Account is locked and cannot be used.',
        '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 11);

    -- Account: Unlocked (positive state — gray is reserved for inactive/negative)
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_unlocked', 'Unlocked', 'Account is unlocked and accessible.',
        '#22c55e', '#ffffff', 'success', 'badge bg-success', 12);

    -- Compute task outcome: Success
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'outcome_success', 'Success', 'Task completed successfully.',
        '#22c55e', '#ffffff', 'success', 'badge bg-success', 13);

    -- Compute task outcome: Failed
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'outcome_failed', 'Failed', 'Task failed to complete.',
        '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 14);

    -- Compute task outcome: No Reply
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'outcome_no_reply', 'No Reply', 'Task received no reply from worker.',
        '#eab308', '#ffffff', 'warning', 'badge bg-warning', 15);

    -- Compute task state: Running
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'state_running', 'Running', 'Task is currently executing.',
        '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 16);

    -- Compute task state: Done (completed — gray is reserved for inactive/negative)
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'state_done', 'Done', 'Task has completed.',
        '#22c55e', '#ffffff', 'success', 'badge bg-success', 17);

    -- Concurrency policy: Skip
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'policy_skip', 'Skip', 'Skip overlapping executions.',
        '#eab308', '#ffffff', 'warning', 'badge bg-warning', 18);

    -- Concurrency policy: Queue
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'policy_queue', 'Queue', 'Queue overlapping executions.',
        '#3b82f6', '#ffffff', 'info', 'badge bg-info', 19);

    -- Concurrency policy: Fail
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'policy_fail', 'Fail', 'Fail on overlapping executions.',
        '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 20);

    -- Report FSM: Draft
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'fsm_draft', 'Draft', 'Report definition is in draft state.',
        '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 21);

    -- Report FSM: Active (reuses 'active' badge for consistency)
    -- (mapped via badge_mappings to existing 'active' badge)

    -- Report FSM: Suspended
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'fsm_suspended', 'Suspended', 'Report definition is suspended.',
        '#eab308', '#ffffff', 'warning', 'badge bg-warning', 22);

    -- Archived (generic; shared by workspace, report definitions, etc.)
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'archived', 'Archived', 'Record has been archived and is no longer active.',
        '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 23);

    -- DQ Origin: Primary
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'origin_primary', 'Primary', 'Data originates from a primary source.',
        '#3b82f6', '#ffffff', 'info', 'badge bg-info', 24);

    -- DQ Origin: Derived
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'origin_derived', 'Derived', 'Data is derived from another source.',
        '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 25);

    -- DQ Nature: Actual
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'nature_actual', 'Actual', 'Real-world data.',
        '#22c55e', '#ffffff', 'success', 'badge bg-success', 26);

    -- DQ Nature: Estimated
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'nature_estimated', 'Estimated', 'Data is an estimation.',
        '#eab308', '#ffffff', 'warning', 'badge bg-warning', 27);

    -- DQ Nature: Simulated
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'nature_simulated', 'Simulated', 'Data is simulated.',
        '#ec4899', '#ffffff', 'primary', 'badge bg-primary', 28);

    -- DQ Treatment: Raw
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'treatment_raw', 'Raw', 'Data has not been processed.',
        '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 29);

    -- DQ Treatment: Cleaned
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'treatment_cleaned', 'Cleaned', 'Data has been cleaned.',
        '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 30);

    -- DQ Treatment: Enriched
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'treatment_enriched', 'Enriched', 'Data has been enriched with additional context.',
        '#a855f7', '#ffffff', 'primary', 'badge bg-primary', 31);

    -- Tenant Status: Bootstrapping (provisioning wizards not yet run)
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenant_bootstrapping', 'Bootstrapping', 'Tenant is awaiting initial provisioning wizard run.',
        '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 32);

    -- Account Type: User
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_type_user', 'User', 'Human user account.',
        '#3b82f6', '#ffffff', 'info', 'badge bg-info', 33);

    -- Account Type: Service (teal — gray is reserved for inactive/negative)
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_type_service', 'Service', 'Service account for non-human processes.',
        '#14b8a6', '#ffffff', 'info', 'badge bg-info', 34);

    -- Account Type: Algorithm
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_type_algorithm', 'Algorithm', 'Account for automated algorithms.',
        '#eab308', '#ffffff', 'warning', 'badge bg-warning', 35);

    -- Account Type: LLM
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_type_llm', 'LLM', 'Account for Large Language Model agents.',
        '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 36);

    -- Currency pair classification: a purely descriptive liquidity tier,
    -- not a status -- deliberately NOT green/amber/red (RAG is reserved for
    -- genuine health/outcome/caution semantics; see ux_language.org rule 3).
    -- Dedicated (not shared with other domains) so this stays true even if
    -- an unrelated domain's badge colour changes later. Four clearly
    -- separated hues (blue/teal/violet/sky), not adjacent shades of the
    -- same hue (an earlier version used two violets that read as
    -- near-identical) and not pink (too close to red at this saturation).
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'classification_major', 'Major', 'Major currency pair (most liquid tier).',
        '#3b82f6', '#ffffff', 'info', 'badge bg-info', 37);
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'classification_minor', 'Minor', 'Minor currency pair (mid liquidity tier).',
        '#14b8a6', '#ffffff', 'info', 'badge bg-info', 38);
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'classification_exotic', 'Exotic', 'Exotic currency pair (least liquid tier).',
        '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 39);
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'classification_commodity', 'Commodity', 'Commodity-linked currency pair.',
        '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 40);

    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'nature_fiat', 'Fiat', 'Government-issued currency not backed by a commodity.',
        '#3b82f6', '#ffffff', 'info', 'badge bg-info', 41);
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'nature_commodity_currency', 'Commodity Currency', 'Currency backed by or representing a physical commodity.',
        '#eab308', '#ffffff', 'warning', 'badge bg-warning', 42);
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'nature_synthetic', 'Synthetic', 'Artificially constructed currency or index.',
        '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 43);
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'nature_supranational', 'Supranational', 'Currency issued by a multi-national authority.',
        '#14b8a6', '#ffffff', 'info', 'badge bg-info', 44);

    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tier_g10', 'G10', 'Major currency with deep liquidity and tight spreads.',
        '#22c55e', '#ffffff', 'success', 'badge bg-success', 45);
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tier_emerging', 'Emerging', 'Currency from a developing economy with moderate liquidity.',
        '#3b82f6', '#ffffff', 'info', 'badge bg-info', 46);
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tier_exotic', 'Exotic Tier', 'Thinly traded currency with wide spreads.',
        '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 47);
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tier_frontier', 'Frontier', 'Currency from a frontier market with limited convertibility.',
        '#eab308', '#ffffff', 'warning', 'badge bg-warning', 48);
    PERFORM ores_dq_badge_definitions_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tier_historical', 'Historical', 'Currency no longer in active use.',
        '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 49);

    -- =============================================================================
    -- Code Domains (workspace)
    -- =============================================================================

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'workspace_status', 'Workspace Status',
        'Lifecycle status codes for workspace records.', 16);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_type', 'Account Type',
        'Classification of account types (user, service, algorithm, llm).', 17);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_online', 'Account Online',
        'Boolean online indicator for accounts (Yes, No).', 18);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_classification', 'Currency Pair Classification',
        'Liquidity classification codes for currency pairs (major, minor, exotic, commodity).', 21);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_business_day_convention', 'Currency Pair Convention Business Day Convention',
        'Business day adjustment convention codes for currency pair conventions.', 22);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_spot_relative', 'Currency Pair Convention Spot Relative',
        'Whether forward dates are generated relative to the spot date (Yes/No badge).', 23);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_end_of_month', 'Currency Pair Convention End Of Month',
        'Whether end-of-month convention applies (Yes/No badge).', 24);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'monetary_nature', 'Monetary Nature',
        'Nature classification codes for currencies (fiat, commodity, synthetic, supranational).', 25);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_market_tier', 'Currency Market Tier',
        'Liquidity tier classification codes for currencies (G10, emerging, exotic, frontier, historical).', 26);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenor_kind', 'Tenor Kind',
        'Whether a tenor is a fixed PERIOD duration or a convention-resolved SPECIAL label.', 27);

    PERFORM ores_dq_code_domains_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenor_unit', 'Tenor Unit',
        'Period unit for a tenor''s duration (day/week/month/year), or NONE for a SPECIAL tenor.', 28);

    -- =============================================================================
    -- Badge Mappings
    -- =============================================================================

    -- --- Badge Mappings ---

    -- party_status
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'party_status', 'Active', 'active');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'party_status', 'Inactive', 'inactive');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'party_status', 'Closed', 'inactive');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'party_status', 'Frozen', 'frozen');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'party_status', 'Pending', 'pending');

    -- book_status: only Active/Closed/Frozen exist in
    -- ores_refdata_book_statuses_tbl (refdata_book_statuses_populate.sql);
    -- Inactive/Pending were removed here as they didn't correspond to any
    -- real row and would never actually be selectable.
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'book_status', 'Active', 'active');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'book_status', 'Closed', 'inactive');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'book_status', 'Frozen', 'frozen');

    -- regulatory_book_type (reuses pending/inactive colours as a neutral
    -- distinct pair, same as elsewhere in this file)
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'regulatory_book_type', 'Trading', 'pending');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'regulatory_book_type', 'Banking', 'inactive');

    -- is_sweepable: renders the boolean as a Yes/No badge, same
    -- true/false -> string("true")/string("false") the Qt model already
    -- emits for is_bool columns.
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'is_sweepable', 'true', 'active');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'is_sweepable', 'false', 'inactive');

    -- portfolio_status
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'portfolio_status', 'Active', 'active');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'portfolio_status', 'Inactive', 'inactive');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'portfolio_status', 'Closed', 'inactive');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'portfolio_status', 'Frozen', 'frozen');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'portfolio_status', 'Pending', 'pending');

    -- portfolio_type
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'portfolio_type', 'Virtual', 'type_virtual');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'portfolio_type', 'Physical', 'type_physical');

    -- book_type
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'book_type', 'Trading', 'type_virtual');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'book_type', 'Banking', 'type_physical');

    -- login_status
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'login_status', 'Online', 'login_online');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'login_status', 'Recent', 'login_recent');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'login_status', 'Old', 'login_old');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'login_status', 'Never', 'login_never');

    -- account_locked
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_locked', 'Locked', 'account_locked');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_locked', 'Unlocked', 'account_unlocked');

    -- account_online (boolean online indicator in the account detail
    -- dialog; No reuses 'inactive' — gray is correct for "not online")
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_online', 'Yes', 'login_online');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_online', 'No', 'inactive');

    -- currency_pair_classification: a liquidity tier, not a status -- no
    -- RAG colours (see ux_language.org rule 3). Four dedicated,
    -- non-adjacent hues (blue/teal/violet/sky), one per tier -- see the
    -- classification_* definitions above.
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_classification', 'major', 'classification_major');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_classification', 'minor', 'classification_minor');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_classification', 'exotic', 'classification_exotic');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_classification', 'commodity', 'classification_commodity');

    -- currency_pair_convention_business_day_convention
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_business_day_convention', 'Following', 'active');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_business_day_convention', 'ModifiedFollowing', 'origin_primary');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_business_day_convention', 'Preceding', 'nature_simulated');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_business_day_convention', 'ModifiedPreceding', 'account_type_service');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_business_day_convention', 'Unadjusted', 'inactive');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_business_day_convention', 'HalfMonthModifiedFollowing', 'tenant_bootstrapping');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_business_day_convention', 'Nearest', 'treatment_enriched');

    -- currency_pair_convention_spot_relative
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_spot_relative', 'Yes', 'login_online');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_spot_relative', 'No', 'inactive');

    -- currency_pair_convention_end_of_month
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_end_of_month', 'Yes', 'login_online');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_pair_convention_end_of_month', 'No', 'inactive');

    -- compute_task_outcome
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'compute_task_outcome', 'Success', 'outcome_success');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'compute_task_outcome', 'Failed', 'outcome_failed');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'compute_task_outcome', 'No Reply', 'outcome_no_reply');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'compute_task_outcome', 'Pending', 'pending');

    -- compute_task_state
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'compute_task_state', 'Running', 'state_running');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'compute_task_state', 'Unsent', 'pending');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'compute_task_state', 'Done', 'state_done');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'compute_task_state', 'Inactive', 'inactive');

    -- report_concurrency_policy
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'report_concurrency_policy', 'Skip', 'policy_skip');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'report_concurrency_policy', 'Queue', 'policy_queue');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'report_concurrency_policy', 'Fail', 'policy_fail');

    -- report_fsm_state
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'report_fsm_state', 'Active', 'active');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'report_fsm_state', 'Draft', 'fsm_draft');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'report_fsm_state', 'Suspended', 'fsm_suspended');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'report_fsm_state', 'Archived', 'archived');

    -- dq_origin
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_origin', 'Primary', 'origin_primary');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_origin', 'Derived', 'origin_derived');

    -- dq_nature
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_nature', 'Actual', 'nature_actual');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_nature', 'Estimated', 'nature_estimated');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_nature', 'Simulated', 'nature_simulated');

    -- workspace_status
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'workspace_status', 'active', 'active');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'workspace_status', 'archived', 'archived');

    -- tenant_status
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenant_status', 'bootstrapping', 'tenant_bootstrapping');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenant_status', 'active', 'active');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenant_status', 'suspended', 'frozen');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenant_status', 'terminated', 'archived');

    -- dq_treatment
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_treatment', 'Raw', 'treatment_raw');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_treatment', 'Cleaned', 'treatment_cleaned');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'dq_treatment', 'Enriched', 'treatment_enriched');

    -- account_type
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_type', 'user', 'account_type_user');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_type', 'service', 'account_type_service');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_type', 'algorithm', 'account_type_algorithm');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'account_type', 'llm', 'account_type_llm');

    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'monetary_nature', 'fiat', 'nature_fiat');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'monetary_nature', 'commodity', 'nature_commodity_currency');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'monetary_nature', 'synthetic', 'nature_synthetic');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'monetary_nature', 'supranational', 'nature_supranational');

    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_market_tier', 'g10', 'tier_g10');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_market_tier', 'emerging', 'tier_emerging');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_market_tier', 'exotic', 'tier_exotic');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_market_tier', 'frontier', 'tier_frontier');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'currency_market_tier', 'historical', 'tier_historical');

    -- tenor_kind (reuses type_virtual/type_physical as a neutral distinct
    -- pair, purely to give PERIOD/SPECIAL visually distinct badges -- not
    -- to convey status, same pattern as regulatory_book_type above)
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenor_kind', 'PERIOD', 'type_physical');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenor_kind', 'SPECIAL', 'type_virtual');

    -- tenor_unit (four reused-but-distinct badges for DAY/WEEK/MONTH/YEAR,
    -- purely for visual differentiation; NONE genuinely means "not
    -- applicable" so gray/inactive is a semantic fit, not just reuse)
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenor_unit', 'DAY', 'origin_primary');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenor_unit', 'WEEK', 'nature_simulated');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenor_unit', 'MONTH', 'treatment_enriched');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenor_unit', 'YEAR', 'state_running');
    PERFORM ores_dq_badge_mappings_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenor_unit', 'NONE', 'inactive');

    -- =============================================================================
    -- Summary
    -- =============================================================================
END $$;

\qecho '--- Summary ---'

select 'Badge Severities' as entity, count(*) as count
from ores_dq_badge_severities_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Code Domains', count(*)
from ores_dq_code_domains_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Badge Definitions', count(*)
from ores_dq_badge_definitions_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Badge Mappings', count(*)
from ores_dq_badge_mappings_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
order by entity;
