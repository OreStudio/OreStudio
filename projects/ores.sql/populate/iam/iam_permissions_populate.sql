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
 * Permissions Population Script
 *
 * Seeds the RBAC permissions table with predefined permission codes.
 * This script is idempotent - running it multiple times will not create
 * duplicate entries.
 *
 * Permission naming convention: "component::resource:action"
 * - component: The system component (iam, refdata, variability, etc.)
 * - resource: The entity type (accounts, currencies, etc.)
 * - action: The operation (create, read, update, delete, etc.)
 *
 * Wildcard permissions:
 * - "*" grants all permissions (superuser)
 * - "component::*" grants all permissions within a component
 */

-- =============================================================================
-- IAM Component Permissions
-- =============================================================================

-- Account management permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:create', 'Create new user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:read', 'View user account details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:update', 'Modify user account settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:delete', 'Delete user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:lock', 'Lock user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:unlock', 'Unlock user accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::accounts:reset_password', 'Force password reset on user accounts');

-- Role management permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:create', 'Create new roles');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:read', 'View role details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:update', 'Modify role permissions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:delete', 'Delete roles');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:assign', 'Assign roles to accounts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::roles:revoke', 'Revoke roles from accounts');

-- Tenant management permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:create', 'Create new tenants');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:read', 'View tenant details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:update', 'Modify tenant settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:delete', 'Delete tenants');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:suspend', 'Suspend tenants');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:terminate', 'Terminate tenants');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::tenants:impersonate', 'Access other tenants');

-- Session permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::sessions:read', 'View active sessions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::sessions:write', 'Create sessions (login / service-login)');

-- Login info permissions (read-only audit data)
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::login_info:read', 'View login history and info');

-- IAM component wildcard
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'iam::*', 'Full access to all IAM operations');

-- =============================================================================
-- Reference Data Component Permissions
-- =============================================================================

-- Currency permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currencies:read',   'View currency details');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currencies:write',  'Create and modify currencies');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currencies:delete', 'Delete currencies');

-- Currency market tier permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currency_market_tiers:read',   'View currency market tiers');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currency_market_tiers:write',  'Create and modify currency market tiers');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::currency_market_tiers:delete', 'Delete currency market tiers');

-- Country permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::countries:read',   'View countries');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::countries:write',  'Create and modify countries');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::countries:delete', 'Delete countries');

-- Monetary nature permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::monetary_natures:read',   'View monetary natures');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::monetary_natures:write',  'Create and modify monetary natures');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::monetary_natures:delete', 'Delete monetary natures');

-- Purpose type permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::purpose_types:read',   'View purpose types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::purpose_types:write',  'Create and modify purpose types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::purpose_types:delete', 'Delete purpose types');

-- Rounding type permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::rounding_types:read',   'View rounding types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::rounding_types:write',  'Create and modify rounding types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::rounding_types:delete', 'Delete rounding types');

-- Party permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::parties:read',   'View parties');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::parties:write',  'Create and modify parties');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::parties:delete', 'Delete parties');

-- Party type permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_types:read',   'View party types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_types:write',  'Create and modify party types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_types:delete', 'Delete party types');

-- Party status permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_statuses:read',   'View party statuses');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_statuses:write',  'Create and modify party statuses');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_statuses:delete', 'Delete party statuses');

-- Party identifier scheme permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_id_schemes:read',   'View party identifier schemes');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_id_schemes:write',  'Create and modify party identifier schemes');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_id_schemes:delete', 'Delete party identifier schemes');

-- Party identifier permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_identifiers:read',   'View party identifiers');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_identifiers:write',  'Create and modify party identifiers');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_identifiers:delete', 'Delete party identifiers');

-- Party contact permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_contacts:read',   'View party contacts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_contacts:write',  'Create and modify party contacts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::party_contacts:delete', 'Delete party contacts');

-- Contact type permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::contact_types:read',   'View contact types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::contact_types:write',  'Create and modify contact types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::contact_types:delete', 'Delete contact types');

-- Counterparty permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::counterparties:read',   'View counterparties');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::counterparties:write',  'Create and modify counterparties');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::counterparties:delete', 'Delete counterparties');

-- Counterparty identifier permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::counterparty_identifiers:read',   'View counterparty identifiers');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::counterparty_identifiers:write',  'Create and modify counterparty identifiers');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::counterparty_identifiers:delete', 'Delete counterparty identifiers');

-- Counterparty contact permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::counterparty_contacts:read',   'View counterparty contacts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::counterparty_contacts:write',  'Create and modify counterparty contacts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::counterparty_contacts:delete', 'Delete counterparty contacts');

-- Book permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::books:read',   'View books');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::books:write',  'Create and modify books');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::books:delete', 'Delete books');

-- Book status permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::book_statuses:read',   'View book statuses');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::book_statuses:write',  'Create and modify book statuses');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::book_statuses:delete', 'Delete book statuses');

-- Portfolio permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::portfolios:read',   'View portfolios');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::portfolios:write',  'Create and modify portfolios');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::portfolios:delete', 'Delete portfolios');

-- Business unit permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::business_units:read',   'View business units');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::business_units:write',  'Create and modify business units');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::business_units:delete', 'Delete business units');

-- Business unit type permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::business_unit_types:read',   'View business unit types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::business_unit_types:write',  'Create and modify business unit types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::business_unit_types:delete', 'Delete business unit types');

-- Business centre permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::business_centres:read',   'View business centres');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::business_centres:write',  'Create and modify business centres');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::business_centres:delete', 'Delete business centres');

-- Refdata component wildcard
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'refdata::*', 'Full access to all reference data operations');

-- =============================================================================
-- Variability Component Permissions
-- =============================================================================

-- System settings permissions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'variability::flags:create', 'Create new system settings');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'variability::flags:read', 'View system setting values');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'variability::flags:update', 'Modify system setting values');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'variability::flags:delete', 'Delete system settings');

-- Variability component wildcard
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'variability::*', 'Full access to all variability operations');

-- =============================================================================
-- Data Quality Component Permissions
-- =============================================================================

-- Change reasons
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::change_reasons:read', 'View change reasons');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::change_reasons:write', 'Create and modify change reasons');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::change_reasons:delete', 'Delete change reasons');

-- Change reason categories
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::change_reason_categories:read', 'View change reason categories');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::change_reason_categories:write', 'Create and modify change reason categories');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::change_reason_categories:delete', 'Delete change reason categories');

-- Catalogs
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::catalogs:read', 'View catalogs');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::catalogs:write', 'Create and modify catalogs');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::catalogs:delete', 'Delete catalogs');

-- Data domains
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::data_domains:read', 'View data domains');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::data_domains:write', 'Create and modify data domains');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::data_domains:delete', 'Delete data domains');

-- Subject areas
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::subject_areas:read', 'View subject areas');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::subject_areas:write', 'Create and modify subject areas');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::subject_areas:delete', 'Delete subject areas');

-- Datasets
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::datasets:read', 'View datasets');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::datasets:write', 'Create and modify datasets');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::datasets:delete', 'Delete datasets');

-- Methodologies
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::methodologies:read', 'View methodologies');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::methodologies:write', 'Create and modify methodologies');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::methodologies:delete', 'Delete methodologies');

-- Coding schemes
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::coding_schemes:read', 'View coding schemes');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::coding_schemes:write', 'Create and modify coding schemes');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::coding_schemes:delete', 'Delete coding schemes');

-- Coding scheme authority types
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::coding_scheme_authority_types:read', 'View coding scheme authority types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::coding_scheme_authority_types:write', 'Create and modify coding scheme authority types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::coding_scheme_authority_types:delete', 'Delete coding scheme authority types');

-- Nature dimensions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::nature_dimensions:read', 'View nature dimensions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::nature_dimensions:write', 'Create and modify nature dimensions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::nature_dimensions:delete', 'Delete nature dimensions');

-- Origin dimensions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::origin_dimensions:read', 'View origin dimensions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::origin_dimensions:write', 'Create and modify origin dimensions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::origin_dimensions:delete', 'Delete origin dimensions');

-- Treatment dimensions
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::treatment_dimensions:read', 'View treatment dimensions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::treatment_dimensions:write', 'Create and modify treatment dimensions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::treatment_dimensions:delete', 'Delete treatment dimensions');

-- Dataset bundles
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::dataset_bundles:read', 'View dataset bundles');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::dataset_bundles:write', 'Create and modify dataset bundles');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::dataset_bundles:delete', 'Delete dataset bundles');

-- Dataset bundle members
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::dataset_bundle_members:read', 'View dataset bundle members');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::dataset_bundle_members:write', 'Create and modify dataset bundle members');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::dataset_bundle_members:delete', 'Delete dataset bundle members');

-- Badge permissions (badge severities, code domains, definitions)
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::badges:read',   'View badge definitions and severities');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::badges:write',  'Create and modify badge definitions and severities');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::badges:delete', 'Delete badge definitions and severities');

-- Data Quality component wildcard
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'dq::*', 'Full access to all data quality operations');

-- =============================================================================
-- Assets Component Permissions
-- =============================================================================

select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'assets::images:read',   'View asset images');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'assets::images:write',  'Upload and modify asset images');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'assets::images:delete', 'Delete asset images');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'assets::*', 'Full access to all assets operations');

-- =============================================================================
-- Scheduler Component Permissions
-- =============================================================================

select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'scheduler::job_definitions:read',   'View scheduled jobs');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'scheduler::job_definitions:write',  'Schedule and update jobs');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'scheduler::job_definitions:delete', 'Unschedule jobs');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'scheduler::*', 'Full access to all scheduler operations');

-- =============================================================================
-- Reporting Component Permissions
-- =============================================================================

select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'reporting::report_definitions:read',   'View report definitions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'reporting::report_definitions:write',  'Create and modify report definitions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'reporting::report_definitions:delete', 'Delete report definitions');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'reporting::report_instances:read',     'View report run history');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'reporting::report_instances:write',    'Trigger report runs');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'reporting::report_instances:delete',   'Delete report run records');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'reporting::report_types:read',         'View report types');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'reporting::concurrency_policies:read', 'View concurrency policies');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'reporting::*', 'Full access to all reporting operations');

-- =============================================================================
-- Trading Component Permissions
-- =============================================================================

select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'trading::instruments:read',   'View trading instruments');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'trading::instruments:write',  'Create and modify trading instruments');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'trading::instruments:delete', 'Delete trading instruments');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'trading::trades:read',        'View trades');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'trading::trades:write',       'Create and modify trades');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'trading::trades:delete',      'Delete trades');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'trading::*', 'Full access to all trading operations');

-- =============================================================================
-- Compute Component Permissions
-- =============================================================================

select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'compute::apps:read',     'View compute applications');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'compute::apps:write',    'Register and update compute applications');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'compute::apps:delete',   'Remove compute applications');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'compute::batches:read',  'View compute batch jobs');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'compute::batches:write', 'Submit and manage compute batch jobs');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'compute::hosts:read',    'View compute hosts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'compute::hosts:write',   'Register and update compute hosts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'compute::hosts:delete',  'Remove compute hosts');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'compute::*', 'Full access to all compute operations');

-- =============================================================================
-- Telemetry Component Permissions
-- =============================================================================

select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'telemetry::logs:write',    'Write telemetry log entries');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'telemetry::logs:read',     'Read telemetry log entries');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'telemetry::samples:write', 'Write service/NATS health samples');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'telemetry::samples:read',  'Read service/NATS health samples');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'telemetry::*', 'Full access to all telemetry operations');

-- =============================================================================
-- Synthetic Component Permissions
-- =============================================================================

select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'synthetic::*', 'Full access to all synthetic data operations');

-- =============================================================================
-- Workflow Component Permissions
-- =============================================================================

select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'workflow::instances:read',   'View workflow instances');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'workflow::instances:write',  'Create workflow instances');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'workflow::instances:delete', 'Delete workflow instances');
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), 'workflow::*', 'Full access to all workflow operations');

-- =============================================================================
-- Global Wildcard Permission
-- =============================================================================

-- Wildcard permission (superuser)
select ores_iam_permissions_upsert_fn(ores_iam_system_tenant_id_fn(), '*', 'Full access to all operations');

-- Show summary
select count(*) as total_permissions from ores_iam_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
