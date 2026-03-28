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

-- =============================================================================
-- Badge Severities
-- =============================================================================

\echo '--- Badge Severities ---'

select ores_dq_badge_severities_upsert_fn(ores_iam_system_tenant_id_fn(),
    'secondary', 'Secondary', 'Muted or neutral state. No special attention required.', 1);

select ores_dq_badge_severities_upsert_fn(ores_iam_system_tenant_id_fn(),
    'info', 'Info', 'Informational state. General context, no action needed.', 2);

select ores_dq_badge_severities_upsert_fn(ores_iam_system_tenant_id_fn(),
    'success', 'Success', 'Positive or active state. Everything is in order.', 3);

select ores_dq_badge_severities_upsert_fn(ores_iam_system_tenant_id_fn(),
    'warning', 'Warning', 'Caution state. Attention may be required.', 4);

select ores_dq_badge_severities_upsert_fn(ores_iam_system_tenant_id_fn(),
    'danger', 'Danger', 'Error or critical state. Immediate attention required.', 5);

select ores_dq_badge_severities_upsert_fn(ores_iam_system_tenant_id_fn(),
    'primary', 'Primary', 'Highlighted or primary action state.', 6);

-- =============================================================================
-- Code Domains
-- =============================================================================

\echo '--- Code Domains ---'

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'party_status', 'Party Status',
    'Lifecycle status codes for party and counterparty records.', 1);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'book_status', 'Book Status',
    'Lifecycle status codes for book records.', 2);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'portfolio_status', 'Portfolio Status',
    'Lifecycle status codes for portfolio records.', 3);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'portfolio_type', 'Portfolio Type',
    'Type codes for portfolio records (Virtual, Physical).', 4);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'book_type', 'Book Type',
    'Type codes for book records (Trading, Banking).', 5);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'login_status', 'Login Status',
    'Login activity status for user accounts (Never, Old, Recent, Online).', 6);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'account_locked', 'Account Locked',
    'Account lock status (Locked, Unlocked).', 7);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'compute_task_state', 'Compute Task State',
    'State codes for compute task records.', 8);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'compute_task_outcome', 'Compute Task Outcome',
    'Outcome codes for completed compute tasks.', 9);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'report_concurrency_policy', 'Report Concurrency Policy',
    'Concurrency policy codes for report definitions (Skip, Queue, Fail).', 10);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'report_fsm_state', 'Report FSM State',
    'FSM lifecycle state codes for report definitions.', 11);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_origin', 'DQ Origin',
    'Data quality origin dimension codes.', 12);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_nature', 'DQ Nature',
    'Data quality nature dimension codes.', 13);

select ores_dq_code_domains_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_treatment', 'DQ Treatment',
    'Data quality treatment dimension codes.', 14);

-- =============================================================================
-- Badge Definitions
-- Colours migrated from ColorConstants.hpp and BadgeColors.hpp.
-- text_colour is always #ffffff (white) for all current badges.
-- =============================================================================

\echo '--- Badge Definitions ---'

-- Status: Active
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'active', 'Active', 'Record is active and operational.',
    '#22c55e', '#ffffff', 'success', 'badge bg-success', 1);

-- Status: Inactive / Closed
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'inactive', 'Inactive', 'Record is inactive or not operational.',
    '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 2);

-- Status: Frozen
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'frozen', 'Frozen', 'Record is frozen; no changes permitted.',
    '#eab308', '#ffffff', 'warning', 'badge bg-warning', 3);

-- Status: Pending
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'pending', 'Pending', 'Record is awaiting processing or approval.',
    '#3b82f6', '#ffffff', 'info', 'badge bg-info', 4);

-- Entity type: Virtual / Portfolio
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'type_virtual', 'Virtual', 'Entity is a virtual construct.',
    '#7c3aed', '#ffffff', 'primary', 'badge bg-primary', 5);

-- Entity type: Physical / Trading
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'type_physical', 'Physical', 'Entity is a physical construct.',
    '#3b82f6', '#ffffff', 'info', 'badge bg-info', 6);

-- Login status: Online
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'login_online', 'Online', 'User is currently logged in.',
    '#22c55e', '#ffffff', 'success', 'badge bg-success', 7);

-- Login status: Recent
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'login_recent', 'Recent', 'User logged in recently.',
    '#3b82f6', '#ffffff', 'info', 'badge bg-info', 8);

-- Login status: Old
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'login_old', 'Old', 'User has not logged in for a long time.',
    '#eab308', '#ffffff', 'warning', 'badge bg-warning', 9);

-- Login status: Never
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'login_never', 'Never', 'User has never logged in.',
    '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 10);

-- Account: Locked
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'account_locked', 'Locked', 'Account is locked and cannot be used.',
    '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 11);

-- Account: Unlocked
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'account_unlocked', 'Unlocked', 'Account is unlocked and accessible.',
    '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 12);

-- Compute task outcome: Success
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'outcome_success', 'Success', 'Task completed successfully.',
    '#22c55e', '#ffffff', 'success', 'badge bg-success', 13);

-- Compute task outcome: Failed
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'outcome_failed', 'Failed', 'Task failed to complete.',
    '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 14);

-- Compute task outcome: No Reply
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'outcome_no_reply', 'No Reply', 'Task received no reply from worker.',
    '#eab308', '#ffffff', 'warning', 'badge bg-warning', 15);

-- Compute task state: Running
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'state_running', 'Running', 'Task is currently executing.',
    '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 16);

-- Compute task state: Done / Inactive
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'state_done', 'Done', 'Task has completed.',
    '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 17);

-- Concurrency policy: Skip
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'policy_skip', 'Skip', 'Skip overlapping executions.',
    '#eab308', '#ffffff', 'warning', 'badge bg-warning', 18);

-- Concurrency policy: Queue
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'policy_queue', 'Queue', 'Queue overlapping executions.',
    '#3b82f6', '#ffffff', 'info', 'badge bg-info', 19);

-- Concurrency policy: Fail
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'policy_fail', 'Fail', 'Fail on overlapping executions.',
    '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 20);

-- Report FSM: Draft
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'fsm_draft', 'Draft', 'Report definition is in draft state.',
    '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 21);

-- Report FSM: Active (reuses 'active' badge for consistency)
-- (mapped via badge_mappings to existing 'active' badge)

-- Report FSM: Suspended
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'fsm_suspended', 'Suspended', 'Report definition is suspended.',
    '#eab308', '#ffffff', 'warning', 'badge bg-warning', 22);

-- Report FSM: Archived
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'fsm_archived', 'Archived', 'Report definition has been archived.',
    '#ef4444', '#ffffff', 'danger', 'badge bg-danger', 23);

-- DQ Origin: Primary
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'origin_primary', 'Primary', 'Data originates from a primary source.',
    '#3b82f6', '#ffffff', 'info', 'badge bg-info', 24);

-- DQ Origin: Derived
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'origin_derived', 'Derived', 'Data is derived from another source.',
    '#8b5cf6', '#ffffff', 'primary', 'badge bg-primary', 25);

-- DQ Nature: Actual
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'nature_actual', 'Actual', 'Real-world data.',
    '#22c55e', '#ffffff', 'success', 'badge bg-success', 26);

-- DQ Nature: Estimated
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'nature_estimated', 'Estimated', 'Data is an estimation.',
    '#eab308', '#ffffff', 'warning', 'badge bg-warning', 27);

-- DQ Nature: Simulated
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'nature_simulated', 'Simulated', 'Data is simulated.',
    '#ec4899', '#ffffff', 'primary', 'badge bg-primary', 28);

-- DQ Treatment: Raw
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'treatment_raw', 'Raw', 'Data has not been processed.',
    '#6b7280', '#ffffff', 'secondary', 'badge bg-secondary', 29);

-- DQ Treatment: Cleaned
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'treatment_cleaned', 'Cleaned', 'Data has been cleaned.',
    '#0ea5e9', '#ffffff', 'info', 'badge bg-info', 30);

-- DQ Treatment: Enriched
select ores_dq_badge_definitions_upsert_fn(ores_iam_system_tenant_id_fn(),
    'treatment_enriched', 'Enriched', 'Data has been enriched with additional context.',
    '#a855f7', '#ffffff', 'primary', 'badge bg-primary', 31);

-- =============================================================================
-- Badge Mappings
-- =============================================================================

\echo '--- Badge Mappings ---'

-- party_status
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'party_status', 'Active', 'active');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'party_status', 'Inactive', 'inactive');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'party_status', 'Closed', 'inactive');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'party_status', 'Frozen', 'frozen');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'party_status', 'Pending', 'pending');

-- book_status
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'book_status', 'Active', 'active');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'book_status', 'Inactive', 'inactive');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'book_status', 'Closed', 'inactive');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'book_status', 'Frozen', 'frozen');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'book_status', 'Pending', 'pending');

-- portfolio_status
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'portfolio_status', 'Active', 'active');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'portfolio_status', 'Inactive', 'inactive');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'portfolio_status', 'Closed', 'inactive');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'portfolio_status', 'Frozen', 'frozen');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'portfolio_status', 'Pending', 'pending');

-- portfolio_type
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'portfolio_type', 'Virtual', 'type_virtual');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'portfolio_type', 'Physical', 'type_physical');

-- book_type
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'book_type', 'Trading', 'type_virtual');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'book_type', 'Banking', 'type_physical');

-- login_status
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'login_status', 'Online', 'login_online');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'login_status', 'Recent', 'login_recent');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'login_status', 'Old', 'login_old');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'login_status', 'Never', 'login_never');

-- account_locked
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'account_locked', 'Locked', 'account_locked');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'account_locked', 'Unlocked', 'account_unlocked');

-- compute_task_outcome
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'compute_task_outcome', 'Success', 'outcome_success');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'compute_task_outcome', 'Failed', 'outcome_failed');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'compute_task_outcome', 'No Reply', 'outcome_no_reply');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'compute_task_outcome', 'Pending', 'pending');

-- compute_task_state
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'compute_task_state', 'Running', 'state_running');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'compute_task_state', 'Unsent', 'pending');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'compute_task_state', 'Done', 'state_done');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'compute_task_state', 'Inactive', 'inactive');

-- report_concurrency_policy
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'report_concurrency_policy', 'Skip', 'policy_skip');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'report_concurrency_policy', 'Queue', 'policy_queue');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'report_concurrency_policy', 'Fail', 'policy_fail');

-- report_fsm_state
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'report_fsm_state', 'Active', 'active');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'report_fsm_state', 'Draft', 'fsm_draft');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'report_fsm_state', 'Suspended', 'fsm_suspended');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'report_fsm_state', 'Archived', 'fsm_archived');

-- dq_origin
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_origin', 'Primary', 'origin_primary');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_origin', 'Derived', 'origin_derived');

-- dq_nature
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_nature', 'Actual', 'nature_actual');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_nature', 'Estimated', 'nature_estimated');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_nature', 'Simulated', 'nature_simulated');

-- dq_treatment
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_treatment', 'Raw', 'treatment_raw');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_treatment', 'Cleaned', 'treatment_cleaned');
select ores_dq_badge_mappings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'dq_treatment', 'Enriched', 'treatment_enriched');

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

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
