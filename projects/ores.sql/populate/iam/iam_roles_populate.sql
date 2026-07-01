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
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'SuperAdmin', 'Platform super administrator with tenant management access');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'TenantAdmin', 'Tenant administrator with full access within a tenant');

    -- Create functional roles
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Trading', 'Trading operations - currency read access');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Sales', 'Sales operations - read-only currency access');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Operations', 'Operations - currency management and account viewing');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Support', 'Support - read-only access to all resources and admin screens');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'Viewer', 'Viewer - basic read-only access to domain data');
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'DataPublisher', 'Data Publisher - can publish datasets and bundles to production');

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
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'IAM domain service — full IAM access');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'IamService', 'iam::*');

    -- Reference Data service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'RefdataService', 'Reference Data domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'RefdataService', 'refdata::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'RefdataService', 'iam::tenants:read');

    -- Workspace service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'WorkspaceService', 'Workspace domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkspaceService', 'workspace::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WorkspaceService', 'iam::tenants:read');

    -- Data Quality service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'DqService', 'Data Quality domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DqService', 'dq::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'DqService', 'iam::tenants:read');

    -- Variability service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'VariabilityService', 'Variability domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'VariabilityService', 'variability::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'VariabilityService', 'iam::tenants:read');

    -- Assets service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'AssetsService', 'Assets domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'AssetsService', 'assets::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'AssetsService', 'iam::tenants:read');

    -- Scheduler service: full own-component + tenant read + change reasons read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'SchedulerService', 'Scheduler domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SchedulerService', 'scheduler::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SchedulerService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SchedulerService', 'dq::change_reasons:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SchedulerService', 'dq::change_reason_categories:read');

    -- Reporting service: full own-component + shared reads + scheduler write/delete
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'Reporting domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'reporting::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'dq::change_reasons:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'dq::change_reason_categories:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'scheduler::job_definitions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'scheduler::job_definitions:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ReportingService', 'scheduler::job_definitions:delete');

    -- Telemetry service: full own-component + tenant read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'TelemetryService', 'Telemetry domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TelemetryService', 'telemetry::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TelemetryService', 'iam::tenants:read');

    -- Trading service: full own-component + all refdata reads + change reasons
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'Trading domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'trading::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'refdata::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'dq::change_reasons:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'TradingService', 'dq::change_reason_categories:read');

    -- Compute service: full own-component + tenant read + refdata parties read
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'Compute Grid domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'compute::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeService', 'refdata::*');

    -- Workflow service: full own-component + iam write (for party provisioning saga) +
    -- refdata write (for party creation)
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'WorkflowService', 'Workflow orchestration domain service');
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
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'Synthetic data generation service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'synthetic::*');
    -- Needs to create/resolve market series when starting a feed.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'marketdata::series:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'marketdata::series:read');
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
    -- Synthetic produces market data: it bootstraps the series catalogue and
    -- persists generated observations via the marketdata service.
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'marketdata::series:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'marketdata::series:write');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'marketdata::observations:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'SyntheticService', 'marketdata::observations:write');

    -- ORE Import service: workflow management + delegated refdata/trading writes
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'ORE Import workflow domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'workflow::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'OreService', 'iam::tenants:read');

    -- Market data service: full access to market data domain
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'MarketdataService', 'Market data domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'MarketdataService', 'marketdata::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'MarketdataService', 'iam::tenants:read');

    -- Controller service: full own-component access (system-level, no tenant isolation)
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'ControllerService', 'Service lifecycle controller');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ControllerService', 'controller::*');

    -- Analytics service: full own-component access
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'AnalyticsService', 'Analytics pricing engine domain service');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'AnalyticsService', 'analytics::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'AnalyticsService', 'iam::tenants:read');

    -- HTTP server service: gateway that validates sessions and forwards to domain services
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'HTTP REST API server — session validation and domain gateway');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'iam::sessions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'HttpService', 'iam::accounts:read');

    -- Wt web service: browser UI that validates sessions and calls domain services via NATS
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'WtService', 'Wt web application — session validation and domain gateway');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WtService', 'iam::tenants:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WtService', 'iam::sessions:read');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'WtService', 'iam::accounts:read');

    -- Compute Wrapper service: worker that processes compute jobs from JetStream
    PERFORM ores_iam_roles_upsert_fn(ores_utility_system_tenant_id_fn(), 'ComputeWrapperService', 'Compute Wrapper worker service — processes compute grid jobs');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeWrapperService', 'compute::*');
    PERFORM ores_iam_role_permissions_assign_fn(ores_utility_system_tenant_id_fn(), 'ComputeWrapperService', 'iam::tenants:read');
END $$;


-- Show summary
select 'Roles:' as summary, count(*) as count from ores_iam_roles_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Role-Permission assignments:', count(*) from ores_iam_role_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
