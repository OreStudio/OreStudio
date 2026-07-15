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

DO $$
BEGIN
    -- =============================================================================
    -- IAM Component Permissions
    -- =============================================================================

    -- Account management permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::accounts:create', 'Create new user accounts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::accounts:read', 'View user account details');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::accounts:update', 'Modify user account settings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::accounts:delete', 'Delete user accounts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::accounts:lock', 'Lock user accounts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::accounts:unlock', 'Unlock user accounts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::accounts:reset_password', 'Force password reset on user accounts');

    -- Role management permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:create', 'Create new roles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:read', 'View role details');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:update', 'Modify role permissions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:delete', 'Delete roles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:assign', 'Assign roles to accounts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:revoke', 'Revoke roles from accounts');

    -- Tenant management permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:create', 'Create new tenants');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:read', 'View tenant details');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:update', 'Modify tenant settings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:delete', 'Delete tenants');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:suspend', 'Suspend tenants');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:terminate', 'Terminate tenants');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:impersonate', 'Access other tenants');

    -- System-level admin reset permissions (SuperAdmin only)
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::system:reset-tenant', 'Bootstrap-reset a tenant so provisioning wizards re-fire on next login');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::system:reset', 'Reset the entire system to pre-bootstrap state (purges all non-system tenants)');

    -- Session permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::sessions:read', 'View active sessions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::sessions:write', 'Create sessions (login / service-login)');

    -- Login info permissions (read-only audit data)
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::login_info:read', 'View login history and info');

    -- IAM component wildcard
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::*', 'Full access to all IAM operations');

    -- =============================================================================
    -- Reference Data Component Permissions
    -- =============================================================================

    -- Currency permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currencies:read',   'View currency details');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currencies:write',  'Create and modify currencies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currencies:delete', 'Delete currencies');

    -- Currency market tier permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_market_tiers:read',   'View currency market tiers');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_market_tiers:write',  'Create and modify currency market tiers');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_market_tiers:delete', 'Delete currency market tiers');

    -- Country permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::countries:read',   'View countries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::countries:write',  'Create and modify countries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::countries:delete', 'Delete countries');

    -- Monetary nature permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::monetary_natures:read',   'View monetary natures');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::monetary_natures:write',  'Create and modify monetary natures');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::monetary_natures:delete', 'Delete monetary natures');

    -- Purpose type permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::purpose_types:read',   'View purpose types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::purpose_types:write',  'Create and modify purpose types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::purpose_types:delete', 'Delete purpose types');

    -- Rounding type permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::rounding_types:read',   'View rounding types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::rounding_types:write',  'Create and modify rounding types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::rounding_types:delete', 'Delete rounding types');

    -- Party permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::parties:read',   'View parties');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::parties:write',  'Create and modify parties');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::parties:delete', 'Delete parties');

    -- Party type permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_types:read',   'View party types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_types:write',  'Create and modify party types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_types:delete', 'Delete party types');

    -- Party status permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_statuses:read',   'View party statuses');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_statuses:write',  'Create and modify party statuses');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_statuses:delete', 'Delete party statuses');

    -- Party identifier scheme permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_id_schemes:read',   'View party identifier schemes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_id_schemes:write',  'Create and modify party identifier schemes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_id_schemes:delete', 'Delete party identifier schemes');

    -- Party identifier permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_identifiers:read',   'View party identifiers');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_identifiers:write',  'Create and modify party identifiers');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_identifiers:delete', 'Delete party identifiers');

    -- Party contact information permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_contact_informations:read',   'View party contact information');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_contact_informations:write',  'Create and modify party contact information');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_contact_informations:delete', 'Delete party contact information');

    -- Contact type permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::contact_types:read',   'View contact types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::contact_types:write',  'Create and modify contact types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::contact_types:delete', 'Delete contact types');

    -- Counterparty permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::counterparties:read',   'View counterparties');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::counterparties:write',  'Create and modify counterparties');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::counterparties:delete', 'Delete counterparties');

    -- Counterparty identifier permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::counterparty_identifiers:read',   'View counterparty identifiers');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::counterparty_identifiers:write',  'Create and modify counterparty identifiers');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::counterparty_identifiers:delete', 'Delete counterparty identifiers');

    -- Counterparty contact information permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::counterparty_contact_informations:read',   'View counterparty contact information');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::counterparty_contact_informations:write',  'Create and modify counterparty contact information');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::counterparty_contact_informations:delete', 'Delete counterparty contact information');

    -- Book permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::books:read',   'View books');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::books:write',  'Create and modify books');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::books:delete', 'Delete books');

    -- Book status permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::book_statuses:read',   'View book statuses');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::book_statuses:write',  'Create and modify book statuses');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::book_statuses:delete', 'Delete book statuses');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::regulatory_book_types:read',   'View regulatory book types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::regulatory_book_types:write',  'Create and modify regulatory book types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::regulatory_book_types:delete', 'Delete regulatory book types');

    -- Portfolio permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::portfolios:read',   'View portfolios');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::portfolios:write',  'Create and modify portfolios');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::portfolios:delete', 'Delete portfolios');

    -- Business unit permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_units:read',   'View business units');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_units:write',  'Create and modify business units');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_units:delete', 'Delete business units');

    -- Business unit type permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_unit_types:read',   'View business unit types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_unit_types:write',  'Create and modify business unit types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_unit_types:delete', 'Delete business unit types');

    -- Business centre permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_centres:read',   'View business centres');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_centres:write',  'Create and modify business centres');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_centres:delete', 'Delete business centres');

    -- Asset class code permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::asset_class_codes:read',   'View asset class codes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::asset_class_codes:write',  'Create and modify asset class codes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::asset_class_codes:delete', 'Delete asset class codes');

    -- Instrument code permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::instrument_codes:read',   'View instrument codes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::instrument_codes:write',  'Create and modify instrument codes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::instrument_codes:delete', 'Delete instrument codes');

    -- Refdata component wildcard
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::*', 'Full access to all reference data operations');

    -- =============================================================================
    -- Workspace Component Permissions
    -- =============================================================================

    -- Workspace management permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workspace::workspaces:read',        'List and view workspaces');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workspace::workspaces:write',       'Create workspaces and update own workspace metadata');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workspace::workspaces:archive',     'Archive a workspace the caller owns');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workspace::workspaces:archive_any', 'Archive any workspace regardless of ownership');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workspace::live_workspace:archive', 'Archive the Live workspace — highly restricted');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workspace::workspaces:delete',      'Soft-delete a workspace the caller owns and its associated data');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workspace::workspaces:delete_any',  'Soft-delete any workspace regardless of ownership');

    -- Workspace component wildcard
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workspace::*', 'Full access to all workspace operations');

    -- =============================================================================
    -- Variability Component Permissions
    -- =============================================================================

    -- System settings permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'variability::flags:create', 'Create new system settings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'variability::flags:read', 'View system setting values');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'variability::flags:update', 'Modify system setting values');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'variability::flags:delete', 'Delete system settings');

    -- Variability component wildcard
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'variability::*', 'Full access to all variability operations');

    -- =============================================================================
    -- Data Quality Component Permissions
    -- =============================================================================

    -- Change reasons
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::change_reasons:read', 'View change reasons');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::change_reasons:write', 'Create and modify change reasons');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::change_reasons:delete', 'Delete change reasons');

    -- Change reason categories
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::change_reason_categories:read', 'View change reason categories');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::change_reason_categories:write', 'Create and modify change reason categories');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::change_reason_categories:delete', 'Delete change reason categories');

    -- Catalogs
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::catalogs:read', 'View catalogs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::catalogs:write', 'Create and modify catalogs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::catalogs:delete', 'Delete catalogs');

    -- Data domains
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::data_domains:read', 'View data domains');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::data_domains:write', 'Create and modify data domains');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::data_domains:delete', 'Delete data domains');

    -- Subject areas
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::subject_areas:read', 'View subject areas');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::subject_areas:write', 'Create and modify subject areas');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::subject_areas:delete', 'Delete subject areas');

    -- Datasets
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::datasets:read', 'View datasets');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::datasets:write', 'Create and modify datasets');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::datasets:delete', 'Delete datasets');

    -- Methodologies
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::methodologies:read', 'View methodologies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::methodologies:write', 'Create and modify methodologies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::methodologies:delete', 'Delete methodologies');

    -- Coding schemes
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::coding_schemes:read', 'View coding schemes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::coding_schemes:write', 'Create and modify coding schemes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::coding_schemes:delete', 'Delete coding schemes');

    -- Coding scheme authority types
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::coding_scheme_authority_types:read', 'View coding scheme authority types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::coding_scheme_authority_types:write', 'Create and modify coding scheme authority types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::coding_scheme_authority_types:delete', 'Delete coding scheme authority types');

    -- Nature dimensions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::nature_dimensions:read', 'View nature dimensions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::nature_dimensions:write', 'Create and modify nature dimensions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::nature_dimensions:delete', 'Delete nature dimensions');

    -- Origin dimensions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::origin_dimensions:read', 'View origin dimensions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::origin_dimensions:write', 'Create and modify origin dimensions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::origin_dimensions:delete', 'Delete origin dimensions');

    -- Treatment dimensions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::treatment_dimensions:read', 'View treatment dimensions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::treatment_dimensions:write', 'Create and modify treatment dimensions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::treatment_dimensions:delete', 'Delete treatment dimensions');

    -- Dataset bundles
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::dataset_bundles:read', 'View dataset bundles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::dataset_bundles:write', 'Create and modify dataset bundles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::dataset_bundles:delete', 'Delete dataset bundles');

    -- Dataset bundle members
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::dataset_bundle_members:read', 'View dataset bundle members');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::dataset_bundle_members:write', 'Create and modify dataset bundle members');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::dataset_bundle_members:delete', 'Delete dataset bundle members');

    -- Badge permissions (badge severities, code domains, definitions)
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badges:read',   'View badge definitions and severities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badges:write',  'Create and modify badge definitions and severities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badges:delete', 'Delete badge definitions and severities');

    -- Data Quality component wildcard
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::*', 'Full access to all data quality operations');

    -- =============================================================================
    -- Assets Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'assets::images:read',   'View asset images');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'assets::images:write',  'Upload and modify asset images');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'assets::images:delete', 'Delete asset images');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'assets::*', 'Full access to all assets operations');

    -- =============================================================================
    -- Scheduler Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'scheduler::job_definitions:read',   'View scheduled jobs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'scheduler::job_definitions:write',  'Schedule and update jobs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'scheduler::job_definitions:delete', 'Unschedule jobs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'scheduler::*', 'Full access to all scheduler operations');

    -- =============================================================================
    -- Reporting Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_definitions:read',   'View report definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_definitions:write',  'Create and modify report definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_definitions:delete', 'Delete report definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_instances:read',     'View report run history');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_instances:write',    'Trigger report runs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_instances:delete',   'Delete report run records');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_types:read',         'View report types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::concurrency_policies:read', 'View concurrency policies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::*', 'Full access to all reporting operations');

    -- =============================================================================
    -- Trading Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instruments:read',   'View trading instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instruments:write',  'Create and modify trading instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instruments:delete', 'Delete trading instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trades:read',        'View trades');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trades:write',       'Create and modify trades');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trades:delete',      'Delete trades');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::*', 'Full access to all trading operations');

    -- =============================================================================
    -- Compute Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::apps:read',     'View compute applications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::apps:write',    'Register and update compute applications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::apps:delete',   'Remove compute applications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::batches:read',  'View compute batch jobs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::batches:write', 'Submit and manage compute batch jobs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::hosts:read',    'View compute hosts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::hosts:write',   'Register and update compute hosts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::hosts:delete',  'Remove compute hosts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::*', 'Full access to all compute operations');

    -- =============================================================================
    -- Telemetry Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'telemetry::logs:write',    'Write telemetry log entries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'telemetry::logs:read',     'Read telemetry log entries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'telemetry::samples:write', 'Write service/NATS health samples');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'telemetry::samples:read',  'Read service/NATS health samples');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'telemetry::*', 'Full access to all telemetry operations');

    -- =============================================================================
    -- Synthetic Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::*', 'Full access to all synthetic data operations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::market_data_generation_configs:read',   'View market data generation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::market_data_generation_configs:write',  'Create and modify market data generation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::market_data_generation_configs:delete', 'Delete market data generation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::fx_spot_generation_configs:read',   'View FX spot generation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::fx_spot_generation_configs:write',  'Create and modify FX spot generation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::fx_spot_generation_configs:delete', 'Delete FX spot generation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::gmm_components:read',   'View GMM components');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::gmm_components:write',  'Create and modify GMM components');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::gmm_components:delete', 'Delete GMM components');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::ir_curve_generation_configs:read',   'View IR curve generation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::ir_curve_generation_configs:write',  'Create and modify IR curve generation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::ir_curve_generation_configs:delete', 'Delete IR curve generation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::ir_curve_template_entries:read',   'View IR curve template entries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::ir_curve_template_entries:write',  'Create and modify IR curve template entries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'synthetic::ir_curve_template_entries:delete', 'Delete IR curve template entries');

    -- =============================================================================
    -- Workflow Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workflow::instances:read',   'View workflow instances');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workflow::instances:write',  'Create workflow instances');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workflow::instances:delete', 'Delete workflow instances');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workflow::*', 'Full access to all workflow operations');

    -- =============================================================================
    -- Market Data Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::series:read',        'View market data series');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::series:write',       'Create and modify market data series');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::series:delete',      'Delete market data series');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::observations:read',  'View market data observations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::observations:write', 'Create and modify market data observations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::fixings:read',       'View market data fixings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::fixings:write',      'Create and modify market data fixings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::*', 'Full access to all market data operations');

    -- =============================================================================
    -- Controller Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'controller::instances:read',   'View service instance status');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'controller::instances:manage', 'Start, stop, and restart service instances');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'controller::definitions:read',  'View service definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'controller::definitions:write', 'Create and modify service definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'controller::events:read',       'View service lifecycle events');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'controller::*', 'Full access to all controller operations');

    -- =============================================================================
    -- Analytics Permissions
    -- =============================================================================
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_engine_types:read',   'View pricing engine types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_engine_types:write',  'Create and modify pricing engine types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_engine_types:delete', 'Delete pricing engine types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_model_configs:read',   'View pricing model configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_model_configs:write',  'Create and modify pricing model configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_model_configs:delete', 'Delete pricing model configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_model_products:read',   'View pricing model products');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_model_products:write',  'Create and modify pricing model products');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_model_products:delete', 'Delete pricing model products');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_model_product_parameters:read',   'View pricing model product parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_model_product_parameters:write',  'Create and modify pricing model product parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_model_product_parameters:delete', 'Delete pricing model product parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::*', 'Full access to all analytics operations');

    -- =============================================================================
    -- Global Wildcard Permission
    -- =============================================================================

    -- Wildcard permission (superuser)
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), '*', 'Full access to all operations');
END $$;


-- Show summary
select count(*) as total_permissions from ores_iam_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
