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
 * Prerequisites:
 * - permissions_populate.sql must be run first
 */

-- Create platform-level admin roles
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'Platform super administrator with tenant management access');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'TenantAdmin', 'Tenant administrator with full access within a tenant');

-- Create functional roles
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'Trading operations - currency read access');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Sales', 'Sales operations - read-only currency access');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'Operations - currency management and account viewing');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Support', 'Support - read-only access to all resources and admin screens');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'Viewer', 'Viewer - basic read-only access to domain data');
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'Data Publisher - can publish datasets and bundles to production');

-- Assign permissions to SuperAdmin role (platform-level)
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', '*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:create');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:update');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:delete');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:suspend');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:terminate');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SuperAdmin', 'iam::tenants:impersonate');

-- Assign permissions to TenantAdmin role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'TenantAdmin', '*');

-- Assign permissions to Trading role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'refdata::currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Trading', 'variability::flags:read');

-- Assign permissions to Sales role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Sales', 'refdata::currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Sales', 'variability::flags:read');

-- Assign permissions to Operations role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'refdata::currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'refdata::currencies:write');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'refdata::currencies:delete');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'variability::flags:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Operations', 'iam::accounts:read');

-- Assign permissions to Support role
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'iam::accounts:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'refdata::currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'variability::flags:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'iam::login_info:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Support', 'iam::roles:read');

-- Assign permissions to Viewer role (default for new accounts)
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Viewer', 'refdata::currencies:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'Viewer', 'variability::flags:read');

-- Assign permissions to DataPublisher role
-- Read access to browse the data catalog
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'dq::catalogs:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'dq::data_domains:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'dq::subject_areas:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'dq::datasets:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'dq::dataset_bundles:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'dq::dataset_bundle_members:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'dq::coding_schemes:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'dq::methodologies:read');
-- Write access for publishing datasets and bundles
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'dq::datasets:write');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DataPublisher', 'dq::dataset_bundles:write');

-- =============================================================================
-- Service Account Roles (one per NATS domain service)
-- Each service gets its own component wildcard plus specific shared reads.
-- =============================================================================

-- IAM service: full own-component access
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'IamService', 'IAM domain service — full IAM access');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'IamService', 'iam::*');

-- Reference Data service: full own-component + tenant read
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'RefdataService', 'Reference Data domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'RefdataService', 'refdata::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'RefdataService', 'iam::tenants:read');

-- Data Quality service: full own-component + tenant read
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'DqService', 'Data Quality domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DqService', 'dq::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'DqService', 'iam::tenants:read');

-- Variability service: full own-component + tenant read
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'VariabilityService', 'Variability domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'VariabilityService', 'variability::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'VariabilityService', 'iam::tenants:read');

-- Assets service: full own-component + tenant read
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'AssetsService', 'Assets domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'AssetsService', 'assets::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'AssetsService', 'iam::tenants:read');

-- Scheduler service: full own-component + tenant read + change reasons read
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'SchedulerService', 'Scheduler domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SchedulerService', 'scheduler::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SchedulerService', 'iam::tenants:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SchedulerService', 'dq::change_reasons:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SchedulerService', 'dq::change_reason_categories:read');

-- Reporting service: full own-component + shared reads + scheduler write/delete
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'ReportingService', 'Reporting domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'ReportingService', 'reporting::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'ReportingService', 'iam::tenants:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'ReportingService', 'dq::change_reasons:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'ReportingService', 'dq::change_reason_categories:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'ReportingService', 'scheduler::job_definitions:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'ReportingService', 'scheduler::job_definitions:write');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'ReportingService', 'scheduler::job_definitions:delete');

-- Telemetry service: full own-component + tenant read
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'TelemetryService', 'Telemetry domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'TelemetryService', 'telemetry::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'TelemetryService', 'iam::tenants:read');

-- Trading service: full own-component + all refdata reads + change reasons
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'TradingService', 'Trading domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'TradingService', 'trading::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'TradingService', 'iam::tenants:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'TradingService', 'refdata::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'TradingService', 'dq::change_reasons:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'TradingService', 'dq::change_reason_categories:read');

-- Compute service: full own-component + tenant read + refdata parties read
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'ComputeService', 'Compute Grid domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'ComputeService', 'compute::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'ComputeService', 'iam::tenants:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'ComputeService', 'refdata::*');

-- Workflow service: full own-component + iam write (for party provisioning saga) +
-- refdata write (for party creation)
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'WorkflowService', 'Workflow orchestration domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'WorkflowService', 'workflow::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'WorkflowService', 'iam::tenants:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'WorkflowService', 'iam::accounts:create');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'WorkflowService', 'iam::accounts:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'WorkflowService', 'iam::accounts:update');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'WorkflowService', 'iam::roles:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'WorkflowService', 'iam::roles:assign');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'WorkflowService', 'refdata::parties:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'WorkflowService', 'refdata::parties:write');

-- Synthetic service: read access across all domain components for data generation
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'Synthetic data generation service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'synthetic::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'iam::tenants:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'iam::accounts:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'iam::roles:read');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'refdata::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'dq::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'variability::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'assets::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'trading::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'reporting::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'scheduler::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'compute::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'SyntheticService', 'telemetry::*');

-- Market data service: full access to market data domain
select ores_iam_roles_upsert_fn(ores_iam_system_tenant_id_fn(), 'MarketdataService', 'Market data domain service');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'MarketdataService', 'marketdata::*');
select ores_iam_role_permissions_assign_fn(ores_iam_system_tenant_id_fn(), 'MarketdataService', 'iam::tenants:read');

-- Show summary
select 'Roles:' as summary, count(*) as count from ores_iam_roles_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Role-Permission assignments:', count(*) from ores_iam_role_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
