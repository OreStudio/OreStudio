/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
 * Roles Population Script
 *
 * Seeds the RBAC roles table with predefined roles and their permission
 * assignments. This script is idempotent - running it multiple times will
 * not create duplicate entries.
 *
 * Predefined Roles:
 * - SuperAdmin: Platform super administrator (tenant0) with tenant management
 * - TenantAdmin: Tenant administrator with full access within a tenant
 * - Trading: Currency read access for trading operations
 * - Sales: Read-only currency access for sales
 * - Operations: Currency management and account viewing
 * - Support: Read-only access to all resources
 * - Viewer: Basic read-only access (default role for new accounts)
 * - DataPublisher: Can publish datasets and bundles to production
 *
 * Service-account roles (one per backend service, granted to that service's IAM
 * account for service-to-service calls): MarketdataService, SyntheticService,
 * RefdataService, DqService, and the other *Service roles defined below.
 *
 * Prerequisites:
 * - permissions_populate.sql must be run first
 */

DO $$
BEGIN
    -- Create platform-level admin roles
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'Platform super administrator with tenant management access', false);
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'TenantAdmin', 'Tenant administrator with full access within a tenant', false);

    -- Every person account holds Member: the reads every person's screens
    -- need, granted one code at a time. A job role adds only what its job
    -- needs. See Role-Based Access Control.
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Member', 'Member - the reference data vocabulary and the shared screens every person reads');

    -- Create functional roles
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'Trading operations - currency read access');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Sales', 'Sales operations - read-only currency access');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'Operations - currency management and account viewing');
    -- The two book-approval functions the role catalogue carried nobody for.
    -- A book change asks the Controller first and then Finance beside Market
    -- Risk and Operations, and each part names its own decide permission. The
    -- parts and the permissions were seeded; no role held three of the four
    -- codes, so only an administrator could answer them. See the approval
    -- request notes for the parts and the controls note for who answers what.
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Controller', 'Controller - the first independent check on a book, and the accounting identity behind it');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'Finance - the accounting identity, the ledger feed and the closing of a book');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Support', 'Support - read-only access to all resources and admin screens');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'Viewer - basic read-only access to domain data');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'Data Publisher - can publish datasets and bundles to production');
    -- The role a scheduled report run acts with. A person grants it when they
    -- schedule a report, and only if they hold every permission in it; a run
    -- token carries it, narrowed to what the grantor still holds.
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'Report run - what a scheduled report run reads and writes in its party');

    -- ReportRun: the run document, the file types it names every file from, and
    -- the owners' configuration documents it reads, the trades and market data
    -- it gathers, the storage objects it writes and reads, and its compute batch.
    -- It writes the batch and its workunits, then reads both back with the
    -- results to collect them, so it needs the read half as well as the write.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'reporting::run_documents:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'reporting::file_types:read');
    -- The packaging step states the date its run priced at on the run itself,
    -- because it is the only step that knows both the date the run asked for
    -- and the date the gathered data carries. The write is narrowed to the
    -- run's own party by the policy on the table.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'reporting::report_instances:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::curve_configurations:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'analytics::pricing_model_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'analytics::todays_market_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::trades:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'marketdata::market_series:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'storage::objects:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'storage::objects:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'compute::batches:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'compute::batches:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'compute::workunits:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'compute::workunits:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'compute::results:read');
    -- The packaging step reads every convention family and the records a trade names. A run token is this role narrowed to what the person holds, and each read checks its own code.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::average_ois_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::bma_basis_swap_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::bond_yield_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::cds_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::cms_spread_option_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::commodity_forward_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::commodity_future_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::counterparties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::counterparty_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::cross_currency_basis_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::cross_currency_fix_float_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::deposit_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::fra_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::future_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::fx_option_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::ibor_index_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::inflation_swap_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::intraday_power_load_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::netting_set_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::ois_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::overnight_index_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::portfolios:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::swap_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::swap_index_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::tenor_basis_swap_long_shorts:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::tenor_basis_swap_pay_receives:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::tenor_basis_two_swap_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::zero_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::zero_inflation_index_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::bond_issues:read');

    -- The role a feed acts with when it starts from a vintage price. A person
    -- grants it when they first start a collection, and only if they hold both
    -- permissions; a run token carries it, narrowed to what the grantor still
    -- holds.
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'SyntheticFeedRun', 'Synthetic feed run - the series and observations a feed reads to resolve its vintage price');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticFeedRun', 'marketdata::market_series:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticFeedRun', 'marketdata::market_observations:read');

    -- Assign permissions to SuperAdmin role (platform-level)
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', '*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:create');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:update');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:suspend');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:terminate');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:impersonate');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'iam::system:reset-tenant');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'iam::system:reset');
    -- The wildcard above does not cover an installation code, so the read of
    -- the deployment as a whole is granted by name.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'installation::deployment:read');

    -- Assign permissions to TenantAdmin role
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TenantAdmin', '*');

    -- Assign permissions to Member role. Each code is classified as
    -- vocabulary or screen furniture; a business fact or personal data does
    -- not belong here.
    -- Reference data records
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currencies:read');
    -- The grid screens every person reads for the work they may see: the series of the
    -- grid, the usage and the job tree of batches, workunits and results. The party
    -- policies of the compute tables cut the rows to the session's tenant and parties.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'compute::grid_samples:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'compute::batches:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'compute::workunits:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'compute::results:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::countries:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::business_centres:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::trading_account_types:read');
    -- The tenor vocabulary: the tenors the reference data holds, the
    -- conventions they resolve under and the schedule axes a pillar names. The
    -- *Curve blueprints* screen reads the catalogue before it can draw a single
    -- field, and the tenor picker stands on any screen that states a period.
    -- No role held the conventions at all, and the tenors and schedules only
    -- Operations, so the screen answered every person with a refusal.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::tenors:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::tenor_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::tenor_schedules:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::asset_classes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::asset_measures:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::benchmark_rates:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::business_processes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::cashflow_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::entity_classifications:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::local_jurisdictions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::party_relationships:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::person_roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::regulatory_corporate_sectors:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::reporting_regimes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::supervisory_bodies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::party_roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::calendars:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::calendar_rules:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::calendar_exceptions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::calendar_events:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::calendar_dates:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currency_groups:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currency_countries:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currency_calendars:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currency_currency_groups:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currency_pairs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currency_pair_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currency_pair_convention_calendars:read');

    -- Reference data classifications
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::asset_class_codes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::book_purpose_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::book_statuses:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::business_day_convention_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::calendar_names:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::calendar_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::contact_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::convention_kinds:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::convention_kind_terms:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currency_market_tiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currency_pair_classifications:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::curve_roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::day_count_fraction_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::day_counters:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::derivation_kinds:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::diary_entry_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::floating_index_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::ledger_feed_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::leg_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::monetary_natures:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::party_statuses:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::party_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::purpose_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::regulatory_book_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::rounding_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::series_subclass_codes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::tenor_anchors:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::tenor_kinds:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::tenor_resolution_algorithms:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::tenor_units:read');

    -- Shared screen furniture
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'dq::badge_definitions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'dq::badge_mappings:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'dq::change_reasons:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'assets::images:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'variability::flags:read');
    -- The role and permission catalogues are open reads: the models name
    -- their list and get subjects in :open_reads:, so a member reads either
    -- without a code. Member does not hold iam::roles:read, because that same
    -- code gates reading another account's roles.
    --
    -- A member reads the accounts of the people they work with. Row-level
    -- security limits iam::accounts:read to accounts that work in a party the
    -- member's own account works in, so the code is not a read of the tenant
    -- (see ores_iam_actor_party_ids_fn in iam_rls_policies_create.sql).
    -- Credentials, contact information, sign-ins and sessions are other tables
    -- with codes of their own, which Member does not hold.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'iam::accounts:read');
    -- The reporting tree: names, job titles, pictures and who reports to whom.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'iam::organisation:read');

    -- The reports a party runs. Report management's front door shows one row per
    -- definition a party owns, with the health of that definition's latest run,
    -- and every person reads it: a trading desk checks it before the first
    -- number is quoted, and a market risk reader signs off from the same rows.
    --
    -- The three reporting reads are party-scoped by row-level security rather
    -- than by a request field (reporting_rls_policies_create.sql), so each code
    -- is a read of the caller's own parties and never of the tenant. That is
    -- the same exception the account read above states.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::report_definitions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::report_instances:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::report_error_events:read');
    -- Every member reads the result rows of its own tenant. The caller scopes
    -- the read to one run with the filter, and tenant isolation keeps another
    -- tenant's rows out.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::report_result_rows:read');
    -- The health word resolves a run's state id to a state name, and the
    -- schedule cell reads the definition's job in the scheduler.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'dq::fsm_states:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'scheduler::job_instances:read');
    -- A report page lists the configurations the report binds and opens each
    -- one: the binding, the configuration, its kind, and the analytics it holds
    -- with their names. The first three are party-scoped by row-level security,
    -- like the reads above. The two kinds lists are reference data of the tenant.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::report_configurations:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::configurations:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::configuration_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::configuration_analytics:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::analytic_types:read');
    -- Editing one of those definitions is the screen's other half, and the put
    -- handler gates it with this code (report_definition_handler.hpp). It is held
    -- by the two administrative roles and by ReportingService, so before this line
    -- a person could read the list and change nothing on it. The row is
    -- party-scoped by the same policy as the reads above.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::report_definitions:write');
    -- The engine settings screens read the documentation of every setting from
    -- the ore parameters, and read and change a configuration through its central
    -- row and its three parts. Deleting a configuration stays with the
    -- administrative roles. The configuration rows are party-scoped by row-level
    -- security, like the report definitions above.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::ore_parameters:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::ore_parameter_value_domains:read');
    -- The curve configuration screen reads a curve document, which the ORE bundle
    -- import writes as the party's own rows and the report configuration binds.
    -- The sections are the vocabulary the document is built from, so both are
    -- reads a person's screen needs. The rows are party-scoped by row-level
    -- security, like the engine settings above.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::curve_sections:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::curve_configurations:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::engine_settings:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::engine_settings:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::engine_settings_fixings:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::engine_settings_fixings:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::engine_settings_building:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::engine_settings_building:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::engine_settings_execution:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::engine_settings_execution:write');
    -- The screen's list reads the groups it draws and the settings each group
    -- holds before it reads a single configuration: the three parts above are
    -- the values and not the list. Nothing but the tenant administrator's
    -- wildcard held these, so *Engine settings* answered every person with a
    -- refusal while the screen that leads to it was offered to them.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::engine_settings_groups:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'reporting::engine_settings_group_settings:read');
    -- A sensitivity template names two lookups, and the screen that manages the
    -- templates offers both as choices: the five methods a key may spell, and the
    -- asset classes ORE presents a template prefix for. The templates themselves
    -- are read by the pricing engine vocabulary, which Member already reads.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'analytics::sensitivity_methods:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'analytics::sensitivity_asset_classes:read');

    -- Assign permissions to Trading role
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'variability::flags:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trades:write_service_origin');

    -- Market data: reading the party's subscriptions, the source each provider routes to.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::subscriptions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::routes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::providers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::market_data_sources:read');

    -- The market data the desk authors and reads. A trader is the maker of a
    -- curve: the synthetic feed journeys name "a quant or a trader who holds
    -- the synthetic configuration permission", and the curve journeys name the
    -- trader who holds the market-data management role. No other seeded role
    -- held any of these, so the *Synthetic* screen, the *Curve evolution*
    -- screen and the whole market-data area were drawn for nobody but a
    -- tenant administrator.
    --
    -- The two feeds are read for the screen's list and written by the two
    -- authoring journeys, which state the feed as theirs to author. The
    -- folders are how the screen groups the feeds, so it cannot draw its list
    -- without them.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'synthetic::fx_spot_generation_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'synthetic::ir_curve_generation_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'synthetic::folders:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'synthetic::fx_spot_generation_configs:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'synthetic::ir_curve_generation_configs:write');
    -- The curve the desk sees evolve, and the slices it reads through: the
    -- *Watch a curve or surface evolve* journey states the series read, and
    -- the snapshot and observation reads are what the evolution screen draws
    -- its window from.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::curve_snapshots:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::market_series:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::market_observations:read');
    -- *Adjust a curve or surface by hand*: the trader is the maker, and the
    -- correction is the write the journey makes.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::market_observations:write');
    -- *Ask for a publish to be approved*: the trader who owns the curve is the
    -- maker, so the desk asks and the Market Risk decider answers. The decide
    -- code is not here, and that is what makes the publish four eyes: the
    -- Operations role holds inbox::approvals:decide_market_risk.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::curve_bootstrap:publish');
    -- *Check that a curve blueprint builds* names the trader who owns the
    -- blueprint, or the quant who owns the method, as the actor, and running the
    -- test needs this code. Nothing but the tenant administrator's wildcard held
    -- it, so the Check tab refused every person it exists for. The test writes
    -- nothing, which is what separates this code from :republish and :publish.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::curve_bootstrap:compute');
    -- The subject the request authorises, which the maker reads back under
    -- *My requests* and the published record read shows once it moved.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'marketdata::curve_publishes:read');
    -- The people the published screens name. A set carries the account that
    -- published it, and the list, the points, the comparison and the blueprint's
    -- *Published sets* tab draw that account with its picture and its full name,
    -- as the history panel draws any actor. Without this read the directory
    -- answers nothing for the name, so the screens draw a bare username and the
    -- initials of a person whose picture they cannot fetch. The read is the
    -- account read and not the tenant's: row-level security still narrows it to
    -- the parties the reader works in, which is the same exception Member's own
    -- line states.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'iam::accounts:read');
    -- *Maintain the curve blueprints* names a trader holding the market-data
    -- management role, or a quant acting for the desk, as the actor, and the
    -- trader is the maker: the blueprint is theirs to author. The blueprint and
    -- its pillars are two records, so the read, the write and the delete of
    -- each are here. No seeded role held any of the six, so the *Curve
    -- blueprints* screen refused every person with the tenant administrator's
    -- wildcard the only way in.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::ir_curve_bootstrap_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::ir_curve_bootstrap_configs:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::ir_curve_bootstrap_configs:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::ir_curve_bootstrap_pillars:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::ir_curve_bootstrap_pillars:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::ir_curve_bootstrap_pillars:delete');

    -- Assign permissions to Sales role
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Sales', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Sales', 'variability::flags:read');

    -- Market data: reading the party's subscriptions, the source each provider routes to.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Sales', 'marketdata::subscriptions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Sales', 'marketdata::routes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Sales', 'marketdata::providers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Sales', 'marketdata::market_data_sources:read');

-- Assign permissions to Operations role.
    --
    -- Operations is the role that maintains reference data, so it holds the
    -- reads and writes the counterparty onboarding journey and the instrument
    -- convention journey need. Member, which every person holds, carries the
    -- shared taxonomy's reads but not these, so the two roles together are
    -- what makes those screens reachable without a tenant administrator.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currencies:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currencies:delete');

    -- Market data: the party's subscriptions and their routes, and the sources board.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'marketdata::subscriptions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'marketdata::routes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'marketdata::providers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'marketdata::market_data_sources:read');
    -- The currency junctions: a currency's calendars, issuing countries,
    -- currency groups and pair conventions are managed with the currency.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_calendars:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_calendars:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_calendars:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_countries:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_countries:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_countries:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_currency_groups:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_currency_groups:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_currency_groups:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_pair_convention_calendars:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_pair_convention_calendars:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_pair_convention_calendars:delete');

    -- Counterparty onboarding (journey 5): the counterparty itself, its
    -- identifiers and contacts, the centres it deals through, the parties
    -- that may see it, and the agreements and collateral terms beneath it.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparties:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparties:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparty_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparty_identifiers:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparty_identifiers:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparty_contact_informations:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparty_contact_informations:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparty_contact_informations:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparty_business_centres:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparty_business_centres:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparty_business_centres:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_counterparties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_counterparties:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_counterparties:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::netting_agreements:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::netting_agreements:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::netting_agreements:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::netting_sets:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::netting_sets:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::netting_sets:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::netting_set_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::netting_set_identifiers:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::netting_set_identifiers:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::csas:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::csas:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::csas:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::csa_eligible_currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::csa_eligible_currencies:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::csa_eligible_currencies:delete');

    -- The parties those counterparties belong to and are visible to, and the
    -- taxonomy both onboarding forms pick from.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::parties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::parties:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::parties:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_types:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_types:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_statuses:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_statuses:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_statuses:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_identifiers:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_identifiers:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_contact_informations:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_contact_informations:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_contact_informations:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_id_schemes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_id_schemes:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_id_schemes:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_centres:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_centres:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_centres:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::trading_account_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::trading_account_types:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::trading_account_types:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::asset_classes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::asset_classes:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::asset_classes:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::asset_measures:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::asset_measures:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::asset_measures:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::benchmark_rates:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::benchmark_rates:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::benchmark_rates:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_processes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_processes:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_processes:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cashflow_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cashflow_types:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cashflow_types:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::entity_classifications:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::entity_classifications:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::entity_classifications:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::local_jurisdictions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::local_jurisdictions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::local_jurisdictions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_relationships:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_relationships:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_relationships:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::person_roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::person_roles:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::person_roles:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::regulatory_corporate_sectors:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::regulatory_corporate_sectors:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::regulatory_corporate_sectors:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::reporting_regimes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::reporting_regimes:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::reporting_regimes:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::supervisory_bodies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::supervisory_bodies:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::supervisory_bodies:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_roles:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::party_roles:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::contact_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::contact_types:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::contact_types:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_units:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_units:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_units:delete');

    -- Instrument conventions (journey 8): every convention family the picker
    -- offers, so a person can author any of them and not only the ones they
    -- happen to hold.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::average_ois_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::average_ois_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::average_ois_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::bma_basis_swap_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::bma_basis_swap_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::bma_basis_swap_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::bond_yield_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::bond_yield_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::bond_yield_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cds_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cds_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cds_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cms_spread_option_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cms_spread_option_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cms_spread_option_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::commodity_forward_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::commodity_forward_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::commodity_forward_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::commodity_future_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::commodity_future_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::commodity_future_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cross_currency_basis_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cross_currency_basis_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cross_currency_basis_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cross_currency_fix_float_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cross_currency_fix_float_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::cross_currency_fix_float_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_pair_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_pair_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_pair_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::deposit_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::deposit_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::deposit_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::fra_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::fra_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::fra_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::future_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::future_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::future_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::fx_option_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::fx_option_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::fx_option_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::ibor_index_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::ibor_index_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::ibor_index_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::inflation_swap_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::inflation_swap_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::inflation_swap_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::intraday_power_load_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::intraday_power_load_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::intraday_power_load_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::ois_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::ois_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::ois_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::overnight_index_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::overnight_index_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::overnight_index_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::swap_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::swap_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::swap_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::swap_index_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::swap_index_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::swap_index_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::convention_kinds:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::convention_kinds:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::convention_kinds:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::convention_kind_terms:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::convention_kind_terms:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::convention_kind_terms:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_basis_two_swap_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_basis_two_swap_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_basis_two_swap_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_basis_swap_pay_receives:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_basis_swap_pay_receives:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_basis_swap_pay_receives:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_basis_swap_long_shorts:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_basis_swap_long_shorts:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_basis_swap_long_shorts:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::zero_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::zero_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::zero_conventions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::zero_inflation_index_conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::zero_inflation_index_conventions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::zero_inflation_index_conventions:delete');

    -- The term lists every convention's pickers read, and the tenors the
    -- resolution step resolves.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::calendar_names:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::calendar_names:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::calendar_names:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::calendars:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::calendars:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::calendars:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_day_convention_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_day_convention_types:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::business_day_convention_types:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::day_count_fraction_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::day_count_fraction_types:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::day_count_fraction_types:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::floating_index_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::floating_index_types:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::floating_index_types:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::payment_frequencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::payment_frequencies:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::payment_frequencies:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::sub_periods_coupon_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::sub_periods_coupon_types:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::sub_periods_coupon_types:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_pairs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_pairs:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currency_pairs:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::calendar_events:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::calendar_events:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::calendar_events:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_anchors:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_anchors:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_anchors:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_resolution_algorithms:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_resolution_algorithms:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_resolution_algorithms:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_schedules:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_schedules:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenor_schedules:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenors:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenors:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::tenors:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'variability::flags:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'iam::accounts:read');

    -- The market risk function's own screens. The pricing engine journeys name
    -- "a market-risk owner or a tenant administrator" as the actor, and the
    -- seed has no market-risk role: the person who owns the terms the desks
    -- book on is an =Operations= account (the conventions journeys say the
    -- same of the same person, and the demonstration's Head of Market Risk
    -- holds this role). Nothing but the tenant administrator's wildcard held
    -- these, so the *Pricing engines* screen was drawn for nobody else.
    --
    -- The write is the authoring workflow the series of journeys runs: author
    -- a configuration, bring one in from a file, and change how a product is
    -- priced. Deleting a configuration stays with the administrative roles.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_model_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_model_configs:write');
    -- The screen counts the products and the parameters of each configuration,
    -- and the authoring workflow picks from the engines, the models, the
    -- methods, the model-and-engine pairs and the pair settings. Those reads
    -- are the screen's and not a privilege: without them the list answered
    -- with a refusal before it reached the configurations it was allowed to
    -- read. The four writes are the products and the parameters an authoring
    -- journey states, and the pairs those products are priced through.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_model_products:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_model_product_parameters:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_model_engine_pairs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_pair_settings:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_models:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_engine_types:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_methods:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::sensitivity_methods:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::sensitivity_asset_classes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::sensitivity_templates:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::sensitivity_templates:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::sensitivity_templates:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_model_products:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_model_product_parameters:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_model_engine_pairs:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'analytics::pricing_pair_settings:write');
    -- The vocabulary the authoring workflow picks from: the instrument codes a
    -- product is written against, and the value sets and catalogues a parameter
    -- takes its values from. The list needs none of them, the form fills from
    -- all of them, and none is a business fact of the tenant.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::instrument_codes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'reporting::ore_parameter_value_sets:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'reporting::ore_parameter_value_set_values:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'reporting::ore_parameter_value_set_catalogues:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'reporting::ore_parameter_setting_value_sets:read');
    -- The market-risk decision an approval request waits for. *Decide a
    -- publish* and *Decide a convention change* name a person from Market
    -- Risk, other than the requester, and the request's kind carries this
    -- code. The wildcard was the only holder, so no seeded person could
    -- decide and every request raised waited for a tenant administrator.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'inbox::approvals:decide_market_risk');
    -- A decider decides from the subject: which curve, at which as-of, for
    -- which party. The queue row carries no subject, so the decider reads the
    -- kind's own record, and this is the code that read takes.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'marketdata::curve_publishes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'marketdata::market_series:read');
    -- The operations decision the other three kinds carry: an operational
    -- authorisation, a book change and a counterparty change all name this
    -- code. *Operational Authorisation* says Operations, the Middle Office,
    -- releases a trade, and the controls note gives Operations the book's
    -- status, its portfolios and the counterparty links, so the function that
    -- does the work is the function that answers for it. Trading raises and
    -- Operations decides, which is the four eyes the control exists for; the
    -- wildcard held the code alone before this line, so every request the
    -- desks raised waited for a tenant administrator.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'inbox::approvals:decide_operations');
    -- *Manage market data subscriptions* states all three codes and names the
    -- party administrator as the actor. The five roles that read a party's
    -- subscriptions read only: Operations maintains the sources and the
    -- routes they serve, so the write and the delete are its own.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'marketdata::subscriptions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'marketdata::subscriptions:delete');
    -- The curve blueprints read only. A blueprint is the desk's recipe and not
    -- a source or a route Operations maintains, so the writes stay with the
    -- maker; Operations reads the recipe behind a curve it is asked about.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::ir_curve_bootstrap_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::ir_curve_bootstrap_pillars:read');

    -- Assign permissions to Support role
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'iam::accounts:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'variability::flags:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'iam::login_info:read');

    -- Market data: read-only access covers the party's subscriptions and the sources.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'marketdata::subscriptions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'marketdata::routes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'marketdata::providers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'marketdata::market_data_sources:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'marketdata::market_series:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'iam::roles:read');
    -- The curve blueprints read only, as Support reads everything it supports.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'refdata::ir_curve_bootstrap_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'refdata::ir_curve_bootstrap_pillars:read');

    -- Assign permissions to Viewer role (default for new accounts)
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'variability::flags:read');

    -- Market data: reading the party's subscriptions, the source each provider routes to.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'marketdata::subscriptions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'marketdata::routes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'marketdata::providers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'marketdata::market_data_sources:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'marketdata::market_series:read');
    -- The curve blueprints read only: a viewer reads the desk's recipes and
    -- changes none of them.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'refdata::ir_curve_bootstrap_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'refdata::ir_curve_bootstrap_pillars:read');
    -- The publisher the published screens name. A viewer reads the published
    -- curves and the blueprints behind them, and every one of those screens
    -- draws the account that published a set with its picture and its full
    -- name, which is the account read. Operations and Support already hold it,
    -- and Market Risk owners work as Operations, so the deciders could resolve a
    -- name before this line; the read-only roles that see the same screens could
    -- not, and drew a bare username.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'iam::accounts:read');

    -- Assign permissions to DataPublisher role
    -- Read access to browse the data catalog
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'dq::catalogs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'dq::data_domains:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'dq::subject_areas:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'dq::datasets:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'dq::dataset_bundles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'dq::dataset_bundle_members:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'dq::coding_schemes:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'dq::methodologies:read');
    -- Write access for publishing datasets and bundles
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'dq::datasets:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'dq::dataset_bundles:write');

    -- Assign permissions to the Controller role
    -- The first part of a book change, and the part that answers first, so the
    -- role reads the book the request is about before it decides. A part
    -- decider holds no kind's decide code: the queue filters by kind and the
    -- notification names the request, so the read of one request is what lets
    -- the controller open it.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Controller', 'inbox::approvals:decide_controller');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Controller', 'inbox::approval_requests:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Controller', 'inbox::approval_decisions:read');
    -- The reasons a hold names, which the decision panel offers.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Controller', 'inbox::hold_reasons:read');
    -- The rules of the kinds it answers: how many approvals, who decides, and
    -- whether the kind holds.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Controller', 'inbox::approval_kinds:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Controller', 'refdata::books:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Controller', 'refdata::portfolios:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Controller', 'refdata::book_changes:read');

    -- Assign permissions to the Finance role
    -- The accounting identity, the ledger feed and closing a book. The same
    -- read-before-decide as the controller, plus the units and parties whose
    -- accounting the changed book belongs to.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'inbox::approvals:decide_finance');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'inbox::approval_requests:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'inbox::approval_decisions:read');
    -- The reasons a hold names, which the decision panel offers.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'inbox::hold_reasons:read');
    -- The rules of the kinds it answers: how many approvals, who decides, and
    -- whether the kind holds.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'inbox::approval_kinds:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'refdata::books:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'refdata::portfolios:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'refdata::business_units:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'refdata::parties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Finance', 'refdata::book_changes:read');

    -- =============================================================================
    -- Service Account Roles (one per NATS domain service)
    -- Each service gets its own component wildcard plus specific shared reads.
    -- =============================================================================

    -- IAM service: full own-component access
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'IAM domain service — full IAM access', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'iam::*');
    -- Tenant provisioning reads the starting point's data from the components it seeds. Every read checks its code, so each is granted by name.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'dq::datasets:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'refdata::parties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'refdata::party_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'synthetic::folders:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'synthetic::fx_spot_generation_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'synthetic::ir_curve_generation_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'synthetic::market_data_generation_configs:read');

    -- Reference Data service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'RefdataService', 'Reference Data domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'RefdataService', 'refdata::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'RefdataService', 'iam::tenants:read');

    -- Data Quality service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'DqService', 'Data Quality domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DqService', 'dq::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DqService', 'iam::tenants:read');

    -- Variability service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'VariabilityService', 'Variability domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'VariabilityService', 'variability::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'VariabilityService', 'iam::tenants:read');

    -- Assets service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'AssetsService', 'Assets domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'AssetsService', 'assets::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'AssetsService', 'iam::tenants:read');

    -- Scheduler service: full own-component + tenant read + change reasons read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'SchedulerService', 'Scheduler domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SchedulerService', 'scheduler::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SchedulerService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SchedulerService', 'dq::change_reasons:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SchedulerService', 'dq::change_reason_categories:read');

    -- Reporting service: full own-component + shared reads + scheduler write/delete
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'Reporting domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'reporting::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'iam::run_grants:exchange');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'dq::change_reasons:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'dq::report_definitions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'dq::change_reason_categories:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'scheduler::job_definitions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'scheduler::job_definitions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'scheduler::job_definitions:delete');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'storage::objects:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'storage::objects:write');

    -- Telemetry service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'TelemetryService', 'Telemetry domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TelemetryService', 'telemetry::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TelemetryService', 'iam::tenants:read');

    -- Storage service: full own-component + tenant read. It needs the database
    -- only to build the request context the shared service runner hands to a
    -- handler; it holds no tables and touches none.
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'StorageService', 'Object storage service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'StorageService', 'storage::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'StorageService', 'iam::tenants:read');

    -- Inbox service: full own-component + tenant read + account reads. It
    -- resolves a notification raised to a permission into the accounts that
    -- hold the permission.
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'InboxService', 'Approvals and notifications service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'InboxService', 'inbox::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'InboxService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'InboxService', 'iam::accounts:read');
    -- The inbox reads how often to sweep from a system setting, and asks
    -- variability for it over the wire rather than reading its table.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'InboxService', 'variability::system_settings:read');
    -- The inbox owns the expiry job, so it reads the scheduler's jobs to
    -- recognise the one it registered last time and writes it to say how often
    -- it fires. Reading first is what stops a restart adding a second job.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'InboxService', 'scheduler::job_definitions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'InboxService', 'scheduler::job_definitions:write');
    -- The inbox resumes an apply that stopped, so it asks the engine to retry
    -- the run.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'InboxService', 'workflow::workflow_instances:write');

    -- Trading service: full own-component + all refdata reads + change reasons
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'Trading domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'trading::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'refdata::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'dq::change_reasons:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'dq::change_reason_categories:read');

    -- Compute service: full own-component + tenant read + refdata parties read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'Compute Grid domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'compute::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'iam::run_grants:exchange');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'iam::storage_capabilities:mint');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'refdata::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'storage::objects:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'storage::objects:write');

    -- Workflow service: full own-component + iam write (for party provisioning saga) +
    -- refdata write (for party creation)
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'Workflow orchestration domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'workflow::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'iam::accounts:create');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'iam::accounts:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'iam::accounts:update');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'iam::roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'iam::roles:assign');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'refdata::parties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'refdata::parties:write');

    -- Synthetic service: read access across all domain components for data generation
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'Synthetic data generation service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'synthetic::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'iam::accounts:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'iam::roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'refdata::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'dq::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'variability::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'assets::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'trading::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'reporting::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'scheduler::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'compute::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'telemetry::*');
    -- Synthetic saves the feed bindings of the feeds a person starts, with that
    -- person's token. It exchanges a collection's run grant for the token that
    -- reads the series and observations of a vintage. Its ticks reach marketdata
    -- over NATS, not through a handler.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'iam::run_grants:exchange');
    -- The simulator serves a source of the catalogue, so it reads the sources through marketdata,
    -- which owns the table. A generator that starts from a vintage price reads the series and the
    -- observations as the service, since no run token stands in for a person any more.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'marketdata::market_data_sources:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'marketdata::market_series:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'marketdata::market_observations:read');

    -- ORE Import service: workflow management + delegated refdata/trading writes
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'ORE Import workflow domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'workflow::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'iam::run_grants:exchange');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'storage::objects:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'storage::objects:write');
    -- An ORE import and run read the documents and records they resolve. Every read checks its code, so each is granted by name.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'analytics::pricing_model_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'analytics::todays_market_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'refdata::conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'refdata::counterparties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'refdata::counterparty_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'refdata::curve_configurations:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'refdata::netting_set_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'refdata::portfolios:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'reporting::run_documents:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'trading::bond_issues:read');

    -- Market data service: full access to market data domain
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'MarketdataService', 'Market data domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'MarketdataService', 'marketdata::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'MarketdataService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'MarketdataService', 'storage::objects:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'MarketdataService', 'storage::objects:write');
    -- The import reads the currency pairs it files observations under. Every read checks its code, so each is granted by name.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'MarketdataService', 'refdata::currency_pairs:read');

    -- Analytics service: full own-component access
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'AnalyticsService', 'Analytics pricing engine domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'AnalyticsService', 'analytics::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'AnalyticsService', 'iam::tenants:read');

    -- HTTP server service: gateway that validates sessions and forwards to domain services
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'HTTP REST API server — session validation and domain gateway', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'iam::sessions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'iam::sessions:end');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'iam::auth_events:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'iam::geo:lookup');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'iam::accounts:read');

    -- Compute Wrapper service: worker that processes compute jobs from JetStream
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'ComputeWrapperService', 'Compute Wrapper worker service — processes compute grid jobs', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeWrapperService', 'compute::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeWrapperService', 'iam::tenants:read');

-- =============================================================================
-- The trade blotter
--
-- Every code the blotter needs was seeded for its handler and granted to no
-- human role, so each screen opened dark. The split below states who holds
-- what, and why:
--
--   Trading   fronts the book: it reads the whole blotter and owns the writes a
--             trader makes, which are booking, amending, grouping trades into a
--             structure and raising the authorisation request that follows.
--   Operations works the queue the trader's writes raise, so it holds the
--             authorisation request and the inbox approvals, and reads the
--             blotter it is asked about.
--   ReportRun reads the blotter to assemble a report, and writes nothing.
--   Viewer reads and never writes, which is also the negative case the task's
--             proof needs.
--   Member is deliberately left out: a blotter is a desk screen, not one every
--             party member holds.
-- =============================================================================

    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_summaries:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_links:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_activities:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trades:read');
    -- The booking form offers the identifier schemes, so the desk reads them.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_id_types:read');
    -- A trader sees the authorisation raised for their own trade, and which
    -- trade it is about.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_authorisation_requests:read');
    -- A trader records the relationship between two trades, and reads the types
    -- it can be.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_links:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_link_types:read');
    -- A link is recorded against the activity the trade's state names, so the
    -- desk reads the state.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_states:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_bookings:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::bond_instruments:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::bond_issues:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_party_roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::structures:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::structure_members:read');
    -- A structure is known by the identifiers the desk gives it, so the desk
    -- reads and states them.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::structure_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::structure_identifiers:write');
    -- The writes a booking makes: the trade, its identifier, its instrument and
    -- the issue the instrument names. A trader who books holds all four.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trades:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_identifiers:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::bond_instruments:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::bond_issues:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::books:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::portfolios:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::counterparties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::counterparty_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trades:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::structures:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::structure_members:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'trading::trade_authorisation_requests:write');

    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::trade_summaries:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::trade_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::trade_links:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::trade_activities:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::trades:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::trade_bookings:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::bond_instruments:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::bond_issues:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::trade_party_roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::structures:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::structure_members:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::books:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::portfolios:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::counterparties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::trade_authorisation_requests:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'trading::trade_authorisation_requests:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'inbox::approval_requests:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'inbox::approval_requests:write');
    -- The reasons a hold names, which the decision panel offers.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'inbox::hold_reasons:read');
    -- The rules of the kinds it answers: how many approvals, who decides, and
    -- whether the kind holds.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'inbox::approval_kinds:read');

    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::trade_summaries:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::trade_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::trade_links:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::trade_activities:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::trade_bookings:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::bond_instruments:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::trade_party_roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::structures:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::structure_members:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::trade_authorisation_requests:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::books:read');

    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::trade_summaries:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::trade_identifiers:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::trade_links:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::trade_activities:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::trades:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::trade_bookings:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::bond_instruments:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::bond_issues:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::trade_party_roles:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::structures:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'trading::structure_members:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'refdata::books:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'refdata::portfolios:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'refdata::counterparties:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'refdata::counterparty_identifiers:read');
END $$;



-- Show summary
select 'Roles:' as summary, count(*) as count from ores_iam_roles_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Role-Permission assignments:', count(*) from ores_iam_role_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
