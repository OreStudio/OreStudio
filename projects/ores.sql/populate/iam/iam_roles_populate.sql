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
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Support', 'Support - read-only access to all resources and admin screens');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'Viewer - basic read-only access to domain data');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'Data Publisher - can publish datasets and bundles to production');
    -- The role a scheduled report run acts with. A person grants it when they
    -- schedule a report, and only if they hold every permission in it; a run
    -- token carries it, narrowed to what the grantor still holds.
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'Report run - what a scheduled report run reads and writes in its party');

    -- ReportRun: the run document and the owners' configuration documents it
    -- reads, the trades and market data it gathers, the storage objects it
    -- writes and reads, and its compute batch.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'reporting::report_run_setups:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::curve_configurations:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'refdata::conventions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'analytics::pricing_model_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'analytics::todays_market_configs:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'trading::trades:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'marketdata::market_series:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'storage::objects:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'storage::objects:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'compute::batches:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportRun', 'compute::workunits:write');

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

    -- Assign permissions to TenantAdmin role
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TenantAdmin', '*');

    -- Assign permissions to Member role. Each code is classified as
    -- vocabulary or screen furniture; a business fact or personal data does
    -- not belong here.
    -- Reference data records
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::countries:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Member', 'refdata::business_centres:read');
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

    -- Assign permissions to Trading role
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'variability::flags:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'workspace::workspaces:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'workspace::workspaces:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'workspace::workspaces:archive');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'workspace::workspaces:delete');

    -- Assign permissions to Sales role
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Sales', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Sales', 'variability::flags:read');

    -- Assign permissions to Operations role
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currencies:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'refdata::currencies:delete');
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
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'variability::flags:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'iam::accounts:read');

    -- Assign permissions to Support role
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'iam::accounts:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'variability::flags:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'iam::login_info:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Support', 'iam::roles:read');

    -- Assign permissions to Viewer role (default for new accounts)
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'refdata::currencies:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'variability::flags:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'workspace::workspaces:read');

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

    -- =============================================================================
    -- Service Account Roles (one per NATS domain service)
    -- Each service gets its own component wildcard plus specific shared reads.
    -- =============================================================================

    -- IAM service: full own-component access
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'IAM domain service — full IAM access', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'iam::*');
    -- Tenant provisioning reads the starting point's data from the components it seeds. Every read checks its code, so each is granted by name.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'dq::datasets:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'marketdata::feed_bindings:read');
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

    -- Workspace service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'WorkspaceService', 'Workspace domain service', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkspaceService', 'workspace::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkspaceService', 'iam::tenants:read');

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
    -- Synthetic reads series and observations to replay a vintage, which the
    -- marketdata read handlers do not gate, and saves the feed bindings of the
    -- feeds it starts. Its ticks reach marketdata over NATS, not through a
    -- handler.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'marketdata::feed_bindings:write');

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
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'reporting::report_run_setups:read');
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
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'iam::accounts:read');

    -- Compute Wrapper service: worker that processes compute jobs from JetStream
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'ComputeWrapperService', 'Compute Wrapper worker service — processes compute grid jobs', false);
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeWrapperService', 'compute::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeWrapperService', 'iam::tenants:read');
END $$;


-- Show summary
select 'Roles:' as summary, count(*) as count from ores_iam_roles_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Role-Permission assignments:', count(*) from ores_iam_role_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
