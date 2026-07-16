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
 * Code Domains Artefact Population Script
 *
 * Populates the dq_code_domains_artefact_tbl staging table. Mirrors the
 * same 31 domains seeded directly into ores_dq_code_domains_tbl (for the
 * system tenant itself) by dq_badge_system_populate.sql -- kept in sync by
 * hand (extracted programmatically from that file when first written); this
 * script is what makes those rows publishable to other tenants.
 *
 * To publish to a tenant:
 *   SELECT * FROM ores_dq_code_domains_publish_fn(
 *       (SELECT id FROM ores_dq_datasets_tbl WHERE code = 'ore.code_domains' AND valid_to = ores_utility_infinity_timestamp_fn()),
 *       <target_tenant_id>,
 *       'upsert'
 *   );
 */

\echo '--- Code Domains Artefacts ---'

select id as v_dataset_id from ores_dq_datasets_tbl where code = 'ore.code_domains' and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_code_domains_artefact_tbl
where dataset_id = :'v_dataset_id';

insert into ores_dq_code_domains_artefact_tbl (
    tenant_id, dataset_id, code, version, name, description, display_order
)
values
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_status', 0, 'Party Status', 'Lifecycle status codes for party and counterparty records.', 1),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'party_type', 0, 'Party Type', 'Classification codes for party and counterparty records.', 30),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_status', 0, 'Book Status', 'Lifecycle status codes for book records.', 2),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'regulatory_book_type', 0, 'Regulatory Book Type', 'Basel III/IV FRTB trading book / banking book classification a book carries.', 19),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'is_sweepable', 0, 'Sweepable', 'Spot-sweep eligibility flag for a book (Yes, No).', 27),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'portfolio_status', 0, 'Portfolio Status', 'Lifecycle status codes for portfolio records.', 3),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'portfolio_type', 0, 'Portfolio Type', 'Type codes for portfolio records (Virtual, Physical).', 4),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'book_type', 0, 'Book Type', 'Type codes for book records (Trading, Banking).', 5),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'login_status', 0, 'Login Status', 'Login activity status for user accounts (Never, Old, Recent, Online).', 6),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_locked', 0, 'Account Locked', 'Account lock status (Locked, Unlocked).', 7),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute_task_state', 0, 'Compute Task State', 'State codes for compute task records.', 8),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'compute_task_outcome', 0, 'Compute Task Outcome', 'Outcome codes for completed compute tasks.', 9),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_concurrency_policy', 0, 'Report Concurrency Policy', 'Concurrency policy codes for report definitions (Skip, Queue, Fail).', 10),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'report_fsm_state', 0, 'Report FSM State', 'FSM lifecycle state codes for report definitions.', 11),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenant_status', 0, 'Tenant Status', 'Lifecycle status codes for tenant records (bootstrapping, active, suspended, terminated).', 15),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_origin', 0, 'DQ Origin', 'Data quality origin dimension codes.', 12),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_nature', 0, 'DQ Nature', 'Data quality nature dimension codes.', 13),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'dq_treatment', 0, 'DQ Treatment', 'Data quality treatment dimension codes.', 14),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'workspace_status', 0, 'Workspace Status', 'Lifecycle status codes for workspace records.', 16),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_type', 0, 'Account Type', 'Classification of account types (user, service, algorithm, llm).', 17),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'account_online', 0, 'Account Online', 'Boolean online indicator for accounts (Yes, No).', 18),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_classification', 0, 'Currency Pair Classification', 'Liquidity classification codes for currency pairs (major, minor, exotic, commodity).', 21),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_business_day_convention', 0, 'Currency Pair Convention Business Day Convention', 'Business day adjustment convention codes for currency pair conventions.', 22),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_spot_relative', 0, 'Currency Pair Convention Spot Relative', 'Whether forward dates are generated relative to the spot date (Yes/No badge).', 23),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_pair_convention_end_of_month', 0, 'Currency Pair Convention End Of Month', 'Whether end-of-month convention applies (Yes/No badge).', 24),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'monetary_nature', 0, 'Monetary Nature', 'Nature classification codes for currencies (fiat, commodity, synthetic, supranational).', 25),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'currency_market_tier', 0, 'Currency Market Tier', 'Liquidity tier classification codes for currencies (G10, emerging, exotic, frontier, historical).', 26),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_kind', 0, 'Tenor Kind', 'Whether a tenor is a fixed PERIOD duration or a convention-resolved SPECIAL label.', 27),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'tenor_unit', 0, 'Tenor Unit', 'Period unit for a tenor''s duration (day/week/month/year), or NONE for a SPECIAL tenor.', 28),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'crm_enabled', 0, 'CRM Enabled', 'Whether a CRM topology config, driver pair, or enabled derived pair is currently active (Yes/No badge); shared across all three crm_* entities.', 29),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id', 'asset_class', 0, 'Asset Class', 'Top-level product classification codes (fx, rates, credit, equity, commodity, inflation, bond, cross_asset), shown on instrument_code and asset_class_code.', 30);
