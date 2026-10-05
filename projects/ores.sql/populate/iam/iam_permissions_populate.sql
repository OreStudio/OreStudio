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

    -- Role management permissions. The generated role write checks
    -- iam::roles:write, and the bundle write checks iam::roles:update, whose
    -- description already names the permission list rather than the role.
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:create', 'Create new roles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:read', 'View role details');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:write', 'Create and modify roles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:update', 'Modify role permissions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:delete', 'Delete roles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:assign', 'Assign roles to accounts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::roles:revoke', 'Revoke roles from accounts');

    -- Permission catalogue permissions. The generated permission CRUD checks
    -- these two; the catalogue itself is seeded, so a screen picks from it
    -- rather than creating a code nothing would enforce.
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::permissions:write', 'Create and modify permissions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::permissions:delete', 'Delete permissions');

    -- Tenant management permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:create', 'Create new tenants');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:read', 'View tenant details');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:update', 'Modify tenant settings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:delete', 'Delete tenants');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:suspend', 'Suspend tenants');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:terminate', 'Terminate tenants');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::tenants:impersonate', 'Access other tenants');

    -- Party provisioning: the party stage a tenant administrator runs inside the
    -- tenant they already work in, which is a narrower act than creating one.
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::parties:provision', 'Provision a party of the tenant the caller works in');

    -- System-level admin reset permissions (SuperAdmin only)
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::system:reset-tenant', 'Bootstrap-reset a tenant so provisioning wizards re-fire on next login');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::system:reset', 'Reset the entire system to pre-bootstrap state (purges all non-system tenants)');

    -- Session permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::sessions:read', 'View active sessions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::sessions:write', 'Create sessions (login / service-login)');

    -- Login info permissions (read-only audit data)
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::login_info:read', 'View login history and info');

    -- Seed profile permissions. The generated handlers check these two codes:
    -- a read carries no permission check, the same as every other entity's
    -- derived read. The steps and the parameters are what a write to a profile
    -- reaches, so they carry their own pair rather than sharing the profile's.
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::seed_profiles:write', 'Create and modify seed profiles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::seed_profiles:delete', 'Delete seed profiles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::seed_profile_steps:write', 'Create and modify the step kinds a seed profile orders');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::seed_profile_steps:delete', 'Delete a step kind from a seed profile');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::seed_profile_parameters:write', 'Create and modify the parameters a seed profile declares');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'iam::seed_profile_parameters:delete', 'Delete a parameter from a seed profile');

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

    -- Series subclass code permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::series_subclass_codes:read',   'View series subclass codes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::series_subclass_codes:write',  'Create and modify series subclass codes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::series_subclass_codes:delete', 'Delete series subclass codes');

    -- Instrument code permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::instrument_codes:read',   'View instrument codes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::instrument_codes:write',  'Create and modify instrument codes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::instrument_codes:delete', 'Delete instrument codes');

    -- Book purpose types permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::book_purpose_types:read',                  'View book purpose types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::book_purpose_types:write',                 'Create and modify book purpose types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::book_purpose_types:delete',                'Delete book purpose types');

    -- Business day convention types permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_day_convention_types:read',       'View business day convention types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_day_convention_types:write',      'Create and modify business day convention types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::business_day_convention_types:delete',     'Delete business day convention types');

    -- Calendar events permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_events:read',                     'View calendar events');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_events:write',                    'Create and modify calendar events');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_events:delete',                   'Delete calendar events');

    -- Calendar exceptions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_exceptions:read',                 'View calendar exceptions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_exceptions:write',                'Create and modify calendar exceptions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_exceptions:delete',               'Delete calendar exceptions');

    -- Calendar rules permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_rules:read',                      'View calendar rules');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_rules:write',                     'Create and modify calendar rules');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_rules:delete',                    'Delete calendar rules');

    -- Calendar types permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_types:read',                      'View calendar types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_types:write',                     'Create and modify calendar types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_types:delete',                    'Delete calendar types');

    -- Calendars permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendars:read',                           'View calendars');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendars:write',                          'Create and modify calendars');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendars:delete',                         'Delete calendars');

    -- Cds conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cds_conventions:read',                     'View cds conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cds_conventions:write',                    'Create and modify cds conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cds_conventions:delete',                   'Delete cds conventions');

    -- Crm driver pairs permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::crm_driver_pairs:read',                    'View crm driver pairs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::crm_driver_pairs:write',                   'Create and modify crm driver pairs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::crm_driver_pairs:delete',                  'Delete crm driver pairs');

    -- Crm enabled derived pairs permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::crm_enabled_derived_pairs:read',           'View crm enabled derived pairs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::crm_enabled_derived_pairs:write',          'Create and modify crm enabled derived pairs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::crm_enabled_derived_pairs:delete',         'Delete crm enabled derived pairs');

    -- Crm topology configs permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::crm_topology_configs:read',                'View crm topology configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::crm_topology_configs:write',               'Create and modify crm topology configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::crm_topology_configs:delete',              'Delete crm topology configs');

    -- Currency calendars permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_calendars:read',                  'View currency calendars');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_calendars:write',                 'Create and modify currency calendars');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_calendars:delete',                'Delete currency calendars');

    -- Currency countries permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_countries:read',                  'View currency countries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_countries:write',                 'Create and modify currency countries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_countries:delete',                'Delete currency countries');

    -- Currency currency groups permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_currency_groups:read',            'View currency currency groups');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_currency_groups:write',           'Create and modify currency currency groups');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_currency_groups:delete',          'Delete currency currency groups');

    -- Currency groups permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_groups:read',                     'View currency groups');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_groups:write',                    'Create and modify currency groups');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_groups:delete',                   'Delete currency groups');

    -- Currency pair classifications permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pair_classifications:read',       'View currency pair classifications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pair_classifications:write',      'Create and modify currency pair classifications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pair_classifications:delete',     'Delete currency pair classifications');

    -- Currency pair convention calendars permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pair_convention_calendars:read',  'View currency pair convention calendars');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pair_convention_calendars:write', 'Create and modify currency pair convention calendars');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pair_convention_calendars:delete', 'Delete currency pair convention calendars');

    -- Currency pair conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pair_conventions:read',           'View currency pair conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pair_conventions:write',          'Create and modify currency pair conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pair_conventions:delete',         'Delete currency pair conventions');

    -- Currency pairs permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pairs:read',                      'View currency pairs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pairs:write',                     'Create and modify currency pairs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::currency_pairs:delete',                    'Delete currency pairs');

    -- Curve roles permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_roles:read',                         'View curve roles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_roles:write',                        'Create and modify curve roles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_roles:delete',                       'Delete curve roles');

    -- Day count fraction types permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::day_count_fraction_types:read',            'View day count fraction types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::day_count_fraction_types:write',           'Create and modify day count fraction types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::day_count_fraction_types:delete',          'Delete day count fraction types');

    -- Deposit conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::deposit_conventions:read',                 'View deposit conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::deposit_conventions:write',                'Create and modify deposit conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::deposit_conventions:delete',               'Delete deposit conventions');

    -- Curve quotes permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_quotes:read',                      'View curve quotes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_quotes:write',                     'Create and modify curve quotes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_quotes:delete',                    'Delete curve quotes');

    -- Curve segments permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_segments:read',                    'View curve segments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_segments:write',                   'Create and modify curve segments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_segments:delete',                  'Delete curve segments');

    -- Curve definitions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_definitions:read',                 'View curve definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_definitions:write',                'Create and modify curve definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_definitions:delete',               'Delete curve definitions');

    -- Netting agreements permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::netting_agreements:read',              'View netting agreements');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::netting_agreements:write',             'Create and modify netting agreements');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::netting_agreements:delete',            'Delete netting agreements');

    -- Netting sets permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::netting_sets:read',                    'View netting sets');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::netting_sets:write',                   'Create and modify netting sets');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::netting_sets:delete',                  'Delete netting sets');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::netting_set_identifiers:read',         'View netting set identifiers');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::netting_set_identifiers:write',        'Create and modify netting set identifiers');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::netting_set_identifiers:delete',       'Delete netting set identifiers');

    -- CSAs permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::csas:read',                            'View CSAs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::csas:write',                           'Create and modify CSAs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::csas:delete',                          'Delete CSAs');

    -- CSA eligible currencies permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::csa_eligible_currencies:read',         'View CSA eligible currencies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::csa_eligible_currencies:write',        'Create and modify CSA eligible currencies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::csa_eligible_currencies:delete',       'Delete CSA eligible currencies');

    -- Portfolio rights permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::portfolio_rights:read',                'View portfolio rights');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::portfolio_rights:write',               'Create and modify portfolio rights');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::portfolio_rights:delete',              'Delete portfolio rights');

    -- Sandboxes permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::sandboxes:read',                       'View sandboxes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::sandboxes:write',                      'Create and modify sandboxes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::sandboxes:delete',                     'Delete sandboxes');

    -- Sandbox members permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::sandbox_members:read',                 'View sandbox members');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::sandbox_members:write',                'Create and modify sandbox members');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::sandbox_members:delete',               'Delete sandbox members');

    -- Curve sections permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_sections:read',                     'View curve sections');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_sections:write',                    'Create and modify curve sections');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_sections:delete',                   'Delete curve sections');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_segment_types:read',               'View curve segment types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_segment_types:write',              'Create and modify curve segment types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_segment_types:delete',             'Delete curve segment types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::day_counters:read',                      'View day counters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::day_counters:write',                     'Create and modify day counters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::day_counters:delete',                    'Delete day counters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_names:read',                    'View calendar names');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_names:write',                   'Create and modify calendar names');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::calendar_names:delete',                  'Delete calendar names');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_security_configs:read',                  'View curve securities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_security_configs:write',                 'Create and modify curve securities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_security_configs:delete',                'Delete curve securities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::intraday_power_curve_configs:read',             'View intraday power curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::intraday_power_curve_configs:write',            'Create and modify intraday power curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::intraday_power_curve_configs:delete',           'Delete intraday power curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::equity_curve_configs:read',                     'View equity curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::equity_curve_configs:write',                    'Create and modify equity curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::equity_curve_configs:delete',                   'Delete equity curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_curve_configs:read',                  'View inflation curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_curve_configs:write',                 'Create and modify inflation curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_curve_configs:delete',                'Delete inflation curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_seasonality_factors:read',     'View inflation seasonality factors');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_seasonality_factors:write',    'Create and modify inflation seasonality factors');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_seasonality_factors:delete',   'Delete inflation seasonality factors');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::default_curve_configs:read',                    'View default curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::default_curve_configs:write',                   'Create and modify default curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::default_curve_configs:delete',                  'Delete default curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::default_curve_configurations:read',      'View default curve configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::default_curve_configurations:write',     'Create and modify default curve configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::default_curve_configurations:delete',    'Delete default curve configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_curve_configs:read',                  'View commodity curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_curve_configs:write',                 'Create and modify commodity curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_curve_configs:delete',                'Delete commodity curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_price_segments:read',          'View commodity price segments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_price_segments:write',         'Create and modify commodity price segments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_price_segments:delete',        'Delete commodity price segments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_report_configurations:read',       'View curve report configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_report_configurations:write',      'Create and modify curve report configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_report_configurations:delete',     'Delete curve report configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::fx_volatility_configs:read',                   'View FX volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::fx_volatility_configs:write',                  'Create and modify FX volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::fx_volatility_configs:delete',                 'Delete FX volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::yield_volatility_configs:read',                'View yield volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::yield_volatility_configs:write',               'Create and modify yield volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::yield_volatility_configs:delete',              'Delete yield volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::base_correlation_configs:read',                 'View base correlations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::base_correlation_configs:write',                'Create and modify base correlations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::base_correlation_configs:delete',               'Delete base correlations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_correlation_configs:read',                'View curve correlations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_correlation_configs:write',               'Create and modify curve correlations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_correlation_configs:delete',              'Delete curve correlations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cds_volatility_configs:read',             'View CDS volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cds_volatility_configs:write',            'Create and modify CDS volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cds_volatility_configs:delete',           'Delete CDS volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cds_volatility_terms:read',         'View CDS volatility terms');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cds_volatility_terms:write',        'Create and modify CDS volatility terms');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cds_volatility_terms:delete',       'Delete CDS volatility terms');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_volatility_configs:read',     'View curve volatility configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_volatility_configs:write',    'Create and modify curve volatility configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_volatility_configs:delete',   'Delete curve volatility configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_cap_floor_volatility_configs:read', 'View inflation cap floor volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_cap_floor_volatility_configs:write', 'Create and modify inflation cap floor volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_cap_floor_volatility_configs:delete', 'Delete inflation cap floor volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_parametric_smiles:read',      'View curve parametric smiles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_parametric_smiles:write',     'Create and modify curve parametric smiles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_parametric_smiles:delete',    'Delete curve parametric smiles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_parametric_smile_parameters:read', 'View curve parametric smile parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_parametric_smile_parameters:write', 'Create and modify curve parametric smile parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_parametric_smile_parameters:delete', 'Delete curve parametric smile parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::swaption_volatility_configs:read',        'View swaption volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::swaption_volatility_configs:write',       'Create and modify swaption volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::swaption_volatility_configs:delete',      'Delete swaption volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cap_floor_volatility_configs:read',       'View cap floor volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cap_floor_volatility_configs:write',      'Create and modify cap floor volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cap_floor_volatility_configs:delete',     'Delete cap floor volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::equity_volatility_configs:read',          'View equity volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::equity_volatility_configs:write',         'Create and modify equity volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::equity_volatility_configs:delete',        'Delete equity volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_volatility_configs:read',       'View commodity volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_volatility_configs:write',      'Create and modify commodity volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_volatility_configs:delete',     'Delete commodity volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::bond_future_volatility_configs:read',     'View bond future volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::bond_future_volatility_configs:write',    'Create and modify bond future volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::bond_future_volatility_configs:delete',   'Delete bond future volatilities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_global_reports:read',         'View curve global reports');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_global_reports:write',        'Create and modify curve global reports');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_global_reports:delete',       'Delete curve global reports');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_configurations:read',              'View curve configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_configurations:write',             'Create and modify curve configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_configurations:delete',            'Delete curve configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::conventions:read',                       'View a conventions document');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::conventions:write',                      'Store a conventions document');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_configuration_sections:read',      'View curve configuration sections');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_configuration_sections:write',     'Create and modify curve configuration sections');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_configuration_sections:delete',    'Delete curve configuration sections');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::yield_curve_configs:read',                      'View yield curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::yield_curve_configs:write',                     'Create and modify yield curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::yield_curve_configs:delete',                    'Delete yield curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_bootstrap_configs:read',           'View curve bootstrap configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_bootstrap_configs:write',          'Create and modify curve bootstrap configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_bootstrap_configs:delete',         'Delete curve bootstrap configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_segment_curves:read',              'View curve segment curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_segment_curves:write',             'Create and modify curve segment curves');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::curve_segment_curves:delete',            'Delete curve segment curves');

    -- Derivation kinds permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::derivation_kinds:read',                    'View derivation kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::derivation_kinds:write',                   'Create and modify derivation kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::derivation_kinds:delete',                  'Delete derivation kinds');

    -- Diary entry types permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::diary_entry_types:read',                   'View diary entry types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::diary_entry_types:write',                  'Create and modify diary entry types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::diary_entry_types:delete',                 'Delete diary entry types');

    -- Floating index types permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::floating_index_types:read',                'View floating index types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::floating_index_types:write',               'Create and modify floating index types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::floating_index_types:delete',              'Delete floating index types');

    -- Fra conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::fra_conventions:read',                     'View fra conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::fra_conventions:write',                    'Create and modify fra conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::fra_conventions:delete',                   'Delete fra conventions');

    -- Ibor index conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ibor_index_conventions:read',              'View ibor index conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ibor_index_conventions:write',             'Create and modify ibor index conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ibor_index_conventions:delete',            'Delete ibor index conventions');

    -- Ir curve bootstrap configs permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ir_curve_bootstrap_configs:read',          'View ir curve bootstrap configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ir_curve_bootstrap_configs:write',         'Create and modify ir curve bootstrap configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ir_curve_bootstrap_configs:delete',        'Delete ir curve bootstrap configs');

    -- Ir curve bootstrap pillars permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ir_curve_bootstrap_pillars:read',          'View ir curve bootstrap pillars');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ir_curve_bootstrap_pillars:write',         'Create and modify ir curve bootstrap pillars');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ir_curve_bootstrap_pillars:delete',        'Delete ir curve bootstrap pillars');

    -- Ledger feed types permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ledger_feed_types:read',                   'View ledger feed types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ledger_feed_types:write',                  'Create and modify ledger feed types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ledger_feed_types:delete',                 'Delete ledger feed types');

    -- Leg types permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::leg_types:read',                           'View leg types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::leg_types:write',                          'Create and modify leg types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::leg_types:delete',                         'Delete leg types');

    -- Ois conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ois_conventions:read',                     'View ois conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ois_conventions:write',                    'Create and modify ois conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::ois_conventions:delete',                   'Delete ois conventions');

    -- Overnight index conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::overnight_index_conventions:read',         'View overnight index conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::overnight_index_conventions:write',        'Create and modify overnight index conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::overnight_index_conventions:delete',       'Delete overnight index conventions');

    -- Party counterparties permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_counterparties:read',                'View party counterparties');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_counterparties:write',               'Create and modify party counterparties');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_counterparties:delete',              'Delete party counterparties');

    -- Party countries permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_countries:read',                     'View party countries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_countries:write',                    'Create and modify party countries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_countries:delete',                   'Delete party countries');

    -- Party currencies permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_currencies:read',                    'View party currencies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_currencies:write',                   'Create and modify party currencies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::party_currencies:delete',                  'Delete party currencies');

    -- Payment frequencies permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::payment_frequencies:read',                 'View payment frequencies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::payment_frequencies:write',                'Create and modify payment frequencies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::payment_frequencies:delete',               'Delete payment frequencies');

    -- Swap conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::swap_conventions:read',                    'View swap conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::swap_conventions:write',                   'Create and modify swap conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::swap_conventions:delete',                  'Delete swap conventions');

    -- Swap index conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::swap_index_conventions:read',              'View swap index conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::swap_index_conventions:write',             'Create and modify swap index conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::swap_index_conventions:delete',            'Delete swap index conventions');

    -- Future conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::future_conventions:read',                  'View future conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::future_conventions:write',                 'Create and modify future conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::future_conventions:delete',                'Delete future conventions');

    -- FX option conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::fx_option_conventions:read',               'View FX option conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::fx_option_conventions:write',              'Create and modify FX option conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::fx_option_conventions:delete',             'Delete FX option conventions');

    -- Averaging OIS conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::average_ois_conventions:read',             'View averaging OIS conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::average_ois_conventions:write',            'Create and modify averaging OIS conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::average_ois_conventions:delete',           'Delete averaging OIS conventions');

    -- Cross-currency basis conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cross_currency_basis_conventions:read',    'View cross-currency basis conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cross_currency_basis_conventions:write',   'Create and modify cross-currency basis conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cross_currency_basis_conventions:delete',  'Delete cross-currency basis conventions');

    -- Two-tenor basis swap conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_basis_two_swap_conventions:read',     'View two-tenor basis swap conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_basis_two_swap_conventions:write',    'Create and modify two-tenor basis swap conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_basis_two_swap_conventions:delete',   'Delete two-tenor basis swap conventions');

    -- Tenor basis swap conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_basis_swap_conventions:read',         'View tenor basis swap conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_basis_swap_conventions:write',        'Create and modify tenor basis swap conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_basis_swap_conventions:delete',       'Delete tenor basis swap conventions');

    -- Zero inflation index conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::zero_inflation_index_conventions:read',     'View zero inflation index conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::zero_inflation_index_conventions:write',    'Create and modify zero inflation index conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::zero_inflation_index_conventions:delete',   'Delete zero inflation index conventions');

    -- BMA basis swap conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::bma_basis_swap_conventions:read',           'View BMA basis swap conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::bma_basis_swap_conventions:write',          'Create and modify BMA basis swap conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::bma_basis_swap_conventions:delete',         'Delete BMA basis swap conventions');

    -- Inflation swap conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_swap_conventions:read',           'View inflation swap conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_swap_conventions:write',          'Create and modify inflation swap conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::inflation_swap_conventions:delete',         'Delete inflation swap conventions');

    -- Cross-currency fix-float conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cross_currency_fix_float_conventions:read', 'View cross-currency fix-float conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cross_currency_fix_float_conventions:write', 'Create and modify cross-currency fix-float conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cross_currency_fix_float_conventions:delete', 'Delete cross-currency fix-float conventions');

    -- CMS spread option conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cms_spread_option_conventions:read',        'View CMS spread option conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cms_spread_option_conventions:write',       'Create and modify CMS spread option conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::cms_spread_option_conventions:delete',      'Delete CMS spread option conventions');

    -- Commodity future conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_future_conventions:read',         'View commodity future conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_future_conventions:write',        'Create and modify commodity future conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_future_conventions:delete',       'Delete commodity future conventions');

    -- Commodity forward conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_forward_conventions:read',        'View commodity forward conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_forward_conventions:write',       'Create and modify commodity forward conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::commodity_forward_conventions:delete',      'Delete commodity forward conventions');

    -- Bond yield conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::bond_yield_conventions:read',               'View bond yield conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::bond_yield_conventions:write',              'Create and modify bond yield conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::bond_yield_conventions:delete',             'Delete bond yield conventions');

    -- Intraday power load conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::intraday_power_load_conventions:read',      'View intraday power load conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::intraday_power_load_conventions:write',     'Create and modify intraday power load conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::intraday_power_load_conventions:delete',    'Delete intraday power load conventions');

    -- Tenor anchors permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_anchors:read',                       'View tenor anchors');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_anchors:write',                      'Create and modify tenor anchors');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_anchors:delete',                     'Delete tenor anchors');

    -- Tenor conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_conventions:read',                   'View tenor conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_conventions:write',                  'Create and modify tenor conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_conventions:delete',                 'Delete tenor conventions');

    -- Tenor kinds permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_kinds:read',                         'View tenor kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_kinds:write',                        'Create and modify tenor kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_kinds:delete',                       'Delete tenor kinds');

    -- Tenor resolution algorithms permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_resolution_algorithms:read',         'View tenor resolution algorithms');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_resolution_algorithms:write',        'Create and modify tenor resolution algorithms');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_resolution_algorithms:delete',       'Delete tenor resolution algorithms');

    -- Tenor schedules permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_schedules:read',                     'View tenor schedules');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_schedules:write',                    'Create and modify tenor schedules');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_schedules:delete',                   'Delete tenor schedules');

    -- Tenor units permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_units:read',                         'View tenor units');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_units:write',                        'Create and modify tenor units');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenor_units:delete',                       'Delete tenor units');

    -- Tenors permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenors:read',                              'View tenors');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenors:write',                             'Create and modify tenors');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::tenors:delete',                            'Delete tenors');

    -- Zero conventions permissions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::zero_conventions:read',                    'View zero conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::zero_conventions:write',                   'Create and modify zero conventions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'refdata::zero_conventions:delete',                  'Delete zero conventions');

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

    -- Artefact types
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::artefact_types:read', 'View artefact types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::artefact_types:write', 'Create and modify artefact types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::artefact_types:delete', 'Delete artefact types');

    -- Badge definitions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badge_definitions:read', 'View badge definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badge_definitions:write', 'Create and modify badge definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badge_definitions:delete', 'Delete badge definitions');

    -- Badge mappings
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badge_mappings:read', 'View badge mappings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badge_mappings:write', 'Create and modify badge mappings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badge_mappings:delete', 'Delete badge mappings');

    -- Badge severities
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badge_severities:read', 'View badge severities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badge_severities:write', 'Create and modify badge severities');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::badge_severities:delete', 'Delete badge severities');

    -- Code domains
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::code_domains:read', 'View code domains');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::code_domains:write', 'Create and modify code domains');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::code_domains:delete', 'Delete code domains');

    -- FSM states
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::fsm_states:read', 'View FSM states');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::fsm_states:write', 'Create and modify FSM states');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::fsm_states:delete', 'Delete FSM states');

    -- FSM transitions
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::fsm_transitions:read', 'View FSM transitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::fsm_transitions:write', 'Create and modify FSM transitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::fsm_transitions:delete', 'Delete FSM transitions');

    -- Publications
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::publications:read', 'View publications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::publications:write', 'Create and modify publications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'dq::publications:delete', 'Delete publications');

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
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'assets::tags:read',   'View asset tags');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'assets::tags:write',  'Create and modify asset tags');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'assets::tags:delete', 'Delete asset tags');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'assets::image_tags:read',   'View image tag associations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'assets::image_tags:write',  'Attach and modify image tag associations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'assets::image_tags:delete', 'Remove image tag associations');
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
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::configuration_types:read', 'View configuration types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::configuration_types:write', 'Create and modify configuration types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::configuration_types:delete', 'Delete configuration types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_type_configuration_types:read', 'View the configuration types report types require');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_type_configuration_types:write', 'Create and modify the configuration types report types require');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_type_configuration_types:delete', 'Delete the configuration types report types require');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::analytic_types:read', 'View analytic types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::analytic_types:write', 'Create and modify analytic types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::analytic_types:delete', 'Delete analytic types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_run_setups:read', 'View report run setups');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_run_setups:write', 'Create and modify report run setups');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_run_setups:delete', 'Delete report run setups');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_analytics:read', 'View report analytics');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_analytics:write', 'Create and modify report analytics');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_analytics:delete', 'Delete report analytics');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_analytic_parameters:read', 'View report analytic parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_analytic_parameters:write', 'Create and modify report analytic parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_analytic_parameters:delete', 'Delete report analytic parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_market_bindings:read', 'View report market bindings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_market_bindings:write', 'Create and modify report market bindings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_market_bindings:delete', 'Delete report market bindings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::configurations:read', 'View configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::configurations:write', 'Create and modify configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::configurations:delete', 'Delete configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_configurations:read', 'View report configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_configurations:write', 'Create and modify report configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_configurations:delete', 'Delete report configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::parameter_value_domains:read', 'View parameter value domains');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::parameter_value_domains:write', 'Create and modify parameter value domains');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::parameter_value_domains:delete', 'Delete parameter value domains');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::parameter_definitions:read', 'View parameter definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::parameter_definitions:write', 'Create and modify parameter definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::parameter_definitions:delete', 'Delete parameter definitions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::configuration_parameters:read', 'View configuration parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::configuration_parameters:write', 'Create and modify configuration parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::configuration_parameters:delete', 'Delete configuration parameters');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::credit_simulation_configs:delete', 'Delete credit simulation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::credit_simulation_configs:write', 'Create and modify credit simulation configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_test_libraries:read', 'View stress test libraries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_test_libraries:write', 'Create and modify stress test libraries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_test_libraries:delete', 'Delete stress test libraries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_test_scenarios:read', 'View stress test scenarios');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_test_scenarios:write', 'Create and modify stress test scenarios');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_test_scenarios:delete', 'Delete stress test scenarios');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_test_shifts:read', 'View stress test shifts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_test_shifts:write', 'Create and modify stress test shifts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_test_shifts:delete', 'Delete stress test shifts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_configs:read', 'View todays market configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_configs:write', 'Create and modify todays market configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_configs:delete', 'Delete todays market configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_collections:read', 'View todays market collections');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_collections:write', 'Create and modify todays market collections');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_collections:delete', 'Delete todays market collections');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_entries:read', 'View todays market entries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_entries:write', 'Create and modify todays market entries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_entries:delete', 'Delete todays market entries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_configurations:read', 'View todays market configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_configurations:write', 'Create and modify todays market configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_configurations:delete', 'Delete todays market configurations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_configuration_bindings:read', 'View todays market configuration bindings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_configuration_bindings:write', 'Create and modify todays market configuration bindings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_configuration_bindings:delete', 'Delete todays market configuration bindings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::credit_simulation_entity_configs:delete', 'Delete credit simulation entity configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::credit_simulation_entity_configs:write', 'Create and modify credit simulation entity configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::credit_simulation_matrix_configs:delete', 'Delete credit simulation matrix configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::credit_simulation_matrix_configs:write', 'Create and modify credit simulation matrix configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::credit_simulation_matrix_row_configs:delete', 'Delete credit simulation matrix row configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::credit_simulation_matrix_row_configs:write', 'Create and modify credit simulation matrix row configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::credit_simulation_netting_set_configs:delete', 'Delete credit simulation netting set configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::credit_simulation_netting_set_configs:write', 'Create and modify credit simulation netting set configs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_types:read',         'View report types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_types:write',        'Create and modify report types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::report_types:delete',       'Delete report types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::concurrency_policies:write',  'Create and modify concurrency policies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::concurrency_policies:delete', 'Delete concurrency policies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::concurrency_policies:read', 'View concurrency policies');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'reporting::*', 'Full access to all reporting operations');

    -- =============================================================================
    -- Trading Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::activity_categories:write',                  'Create and modify activity categories');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::activity_categories:delete',                 'Delete activity categories');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::activity_types:write',                       'Create and modify activity types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::activity_types:delete',                      'Delete activity types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::amortization_types:write',                   'Create and modify amortization types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::amortization_types:delete',                  'Delete amortization types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::ascots:write',                               'Create and modify ascots');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::ascots:delete',                              'Delete ascots');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::average_types:write',                        'Create and modify average types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::average_types:delete',                       'Delete average types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::balance_guaranteed_swap_instruments:write',  'Create and modify balance guaranteed swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::balance_guaranteed_swap_instruments:delete', 'Delete balance guaranteed swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::barrier_types:write',                        'Create and modify barrier types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::barrier_types:delete',                       'Delete barrier types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_forwards:write',                        'Create and modify bond forwards');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_forwards:delete',                       'Delete bond forwards');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_futures:write',                         'Create and modify bond futures');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_futures:delete',                        'Delete bond futures');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_instruments:write',                     'Create and modify bond instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_instruments:delete',                    'Delete bond instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_call_dates:write',                'Create and modify bond issue call dates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_call_dates:delete',               'Delete bond issue call dates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_conversion_targets:write',        'Create and modify bond issue conversion targets');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_conversion_targets:delete',       'Delete bond issue conversion targets');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_leg_amortizations:write',         'Create and modify bond issue leg amortizations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_leg_amortizations:delete',        'Delete bond issue leg amortizations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_leg_amounts:write',               'Create and modify bond issue leg amounts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_leg_amounts:delete',              'Delete bond issue leg amounts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_leg_rates:write',                 'Create and modify bond issue leg rates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_leg_rates:delete',                'Delete bond issue leg rates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_leg_schedule_dates:write',        'Create and modify bond issue leg schedule dates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_leg_schedule_dates:delete',       'Delete bond issue leg schedule dates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_leg_schedules:write',             'Create and modify bond issue leg schedules');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_leg_schedules:delete',            'Delete bond issue leg schedules');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_legs:write',                      'Create and modify bond issue legs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issue_legs:delete',                     'Delete bond issue legs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issues:write',                          'Create and modify bond issues');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_issues:delete',                         'Delete bond issues');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_leg_amortizations:write',               'Create and modify bond leg amortizations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_leg_amortizations:delete',              'Delete bond leg amortizations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_leg_amounts:write',                     'Create and modify bond leg amounts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_leg_amounts:delete',                    'Delete bond leg amounts');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_leg_rates:write',                       'Create and modify bond leg rates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_leg_rates:delete',                      'Delete bond leg rates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_legs:write',                            'Create and modify bond legs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_legs:delete',                           'Delete bond legs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_options:write',                         'Create and modify bond options');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_options:delete',                        'Delete bond options');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_repos:write',                           'Create and modify bond repos');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_repos:delete',                          'Delete bond repos');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_trs:write',                             'Create and modify bond trs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::bond_trs:delete',                            'Delete bond trs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::callable_swap_call_dates:write',             'Create and modify callable swap call dates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::callable_swap_call_dates:delete',            'Delete callable swap call dates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::callable_swap_instruments:write',            'Create and modify callable swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::callable_swap_instruments:delete',           'Delete callable swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::cap_floor_instruments:write',                'Create and modify cap floor instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::cap_floor_instruments:delete',               'Delete cap floor instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::commodity_basket_constituents:write',        'Create and modify commodity basket constituents');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::commodity_basket_constituents:delete',       'Delete commodity basket constituents');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::commodity_instruments:write',                'Create and modify commodity instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::commodity_instruments:delete',               'Delete commodity instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::composite_instruments:write',                'Create and modify composite instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::composite_instruments:delete',               'Delete composite instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::composite_legs:write',                       'Create and modify composite legs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::composite_legs:delete',                      'Delete composite legs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::credit_instruments:write',                   'Create and modify credit instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::credit_instruments:delete',                  'Delete credit instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_accumulator_instruments:write',       'Create and modify equity accumulator instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_accumulator_instruments:delete',      'Delete equity accumulator instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_asian_option_instruments:write',      'Create and modify equity asian option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_asian_option_instruments:delete',     'Delete equity asian option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_barrier_option_instruments:write',    'Create and modify equity barrier option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_barrier_option_instruments:delete',   'Delete equity barrier option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_digital_option_instruments:write',    'Create and modify equity digital option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_digital_option_instruments:delete',   'Delete equity digital option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_forward_instruments:write',           'Create and modify equity forward instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_forward_instruments:delete',          'Delete equity forward instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_option_instruments:write',            'Create and modify equity option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_option_instruments:delete',           'Delete equity option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_position_instruments:write',          'Create and modify equity position instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_position_instruments:delete',         'Delete equity position instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_position_option_underlyings:write',   'Create and modify equity position option underlyings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_position_option_underlyings:delete',  'Delete equity position option underlyings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_swap_instruments:write',              'Create and modify equity swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_swap_instruments:delete',             'Delete equity swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_variance_swap_instruments:write',     'Create and modify equity variance swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::equity_variance_swap_instruments:delete',    'Delete equity variance swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::exercise_types:write',                       'Create and modify exercise types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::exercise_types:delete',                      'Delete exercise types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fpml_event_types:write',                     'Create and modify fpml event types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fpml_event_types:delete',                    'Delete fpml event types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fra_instruments:write',                      'Create and modify fra instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fra_instruments:delete',                     'Delete fra instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_accumulator_instruments:write',           'Create and modify fx accumulator instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_accumulator_instruments:delete',          'Delete fx accumulator instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_asian_forward_instruments:write',         'Create and modify fx asian forward instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_asian_forward_instruments:delete',        'Delete fx asian forward instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_barrier_option_instruments:write',        'Create and modify fx barrier option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_barrier_option_instruments:delete',       'Delete fx barrier option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_digital_option_instruments:write',        'Create and modify fx digital option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_digital_option_instruments:delete',       'Delete fx digital option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_forward_instruments:write',               'Create and modify fx forward instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_forward_instruments:delete',              'Delete fx forward instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_vanilla_option_instruments:write',        'Create and modify fx vanilla option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_vanilla_option_instruments:delete',       'Delete fx vanilla option instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_variance_swap_instruments:write',         'Create and modify fx variance swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::fx_variance_swap_instruments:delete',        'Delete fx variance swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::inflation_swap_instruments:write',           'Create and modify inflation swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::inflation_swap_instruments:delete',          'Delete inflation swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_option_exercise_fees:write',      'Create and modify instrument option exercise fees');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_option_exercise_fees:delete',     'Delete instrument option exercise fees');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_option_payment_dates:write',      'Create and modify instrument option payment dates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_option_payment_dates:delete',     'Delete instrument option payment dates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_option_premiums:write',           'Create and modify instrument option premiums');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_option_premiums:delete',          'Delete instrument option premiums');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_options:write',                   'Create and modify instrument options');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_options:delete',                  'Delete instrument options');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_schedule_dates:write',            'Create and modify instrument schedule dates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_schedule_dates:delete',           'Delete instrument schedule dates');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_schedules:write',                 'Create and modify instrument schedules');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_schedules:delete',                'Delete instrument schedules');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_strikes:write',                   'Create and modify instrument strikes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instrument_strikes:delete',                  'Delete instrument strikes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instruments:read',                           'View trading instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instruments:write',                          'Create and modify trading instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::instruments:delete',                         'Delete trading instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::knock_out_swap_instruments:write',           'Create and modify knock out swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::knock_out_swap_instruments:delete',          'Delete knock out swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::lifecycle_events:write',                     'Create and modify lifecycle events');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::lifecycle_events:delete',                    'Delete lifecycle events');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::long_short_types:write',                     'Create and modify long short types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::long_short_types:delete',                    'Delete long short types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::moment_types:write',                         'Create and modify moment types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::moment_types:delete',                        'Delete moment types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::option_types:write',                         'Create and modify option types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::option_types:delete',                        'Delete option types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::party_role_types:write',                     'Create and modify party role types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::party_role_types:delete',                    'Delete party role types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::payoff_types:write',                         'Create and modify payoff types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::payoff_types:delete',                        'Delete payoff types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::price_types:write',                          'Create and modify price types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::price_types:delete',                         'Delete price types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::return_types:write',                         'Create and modify return types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::return_types:delete',                        'Delete return types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::rpa_instruments:write',                      'Create and modify rpa instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::rpa_instruments:delete',                     'Delete rpa instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::scripted_instruments:write',                 'Create and modify scripted instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::scripted_instruments:delete',                'Delete scripted instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::settlement_types:write',                     'Create and modify settlement types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::settlement_types:delete',                    'Delete settlement types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::swap_legs:write',                            'Create and modify swap legs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::swap_legs:delete',                           'Delete swap legs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::swaption_instruments:write',                 'Create and modify swaption instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::swaption_instruments:delete',                'Delete swaption instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_id_types:write',                       'Create and modify trade id types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_id_types:delete',                      'Delete trade id types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_identifiers:write',                    'Create and modify trade identifiers');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_identifiers:delete',                   'Delete trade identifiers');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_party_roles:write',                    'Create and modify trade party roles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_party_roles:delete',                   'Delete trade party roles');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_bookings:write',                       'Create and modify trade bookings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_bookings:delete',                      'Delete trade bookings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_states:write',                         'Create and modify trade states');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_states:delete',                        'Delete trade states');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_additional_fields:write',              'Create and modify trade additional fields');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_additional_fields:delete',             'Delete trade additional fields');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_portfolios:write',                     'Create and modify trade portfolios');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_portfolios:delete',                    'Delete trade portfolios');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_types:write',                          'Create and modify trade types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trade_types:delete',                         'Delete trade types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trades:read',                                'View trades');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trades:write',                               'Create and modify trades');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::trades:delete',                              'Delete trades');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::vanilla_swap_instruments:write',             'Create and modify vanilla swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::vanilla_swap_instruments:delete',            'Delete vanilla swap instruments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'trading::*', 'Full access to all trading operations');

    -- =============================================================================
    -- Compute Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::apps:read',     'View compute applications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::apps:write',    'Register and update compute applications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::apps:delete',   'Remove compute applications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::app_versions:write',  'Register and update compute application versions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::app_versions:delete', 'Remove compute application versions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::app_version_platforms:write',  'Bind compute application versions to platforms');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::app_version_platforms:delete', 'Unbind compute application versions from platforms');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::platforms:write',  'Register and update compute platforms');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::platforms:delete', 'Remove compute platforms');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::batches:read',  'View compute batch jobs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::batches:write', 'Submit and manage compute batch jobs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::batches:delete', 'Remove compute batch jobs');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::results:write',  'Record compute job outcomes');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::results:delete', 'Remove compute job results');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::workunits:write',  'Create and update compute workunits');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'compute::workunits:delete', 'Remove compute workunits');
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

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workflow::workflow_instances:write',  'Create and modify workflow instances');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workflow::workflow_instances:delete', 'Delete workflow instances');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workflow::workflow_steps:write',      'Create and modify workflow steps');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workflow::workflow_steps:delete',     'Delete workflow steps');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'workflow::*', 'Full access to all workflow operations');

    -- =============================================================================
    -- Market Data Component Permissions
    -- =============================================================================

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::market_series:write',                'Create and modify market data series');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::market_series:delete',               'Delete market data series');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::market_observations:write',          'Create and modify market data observations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::market_observations:delete',         'Delete market data observations');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::market_fixings:write',               'Create and modify market data fixings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::market_fixings:delete',              'Delete market data fixings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::feed_bindings:write',                'Create and modify feed bindings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::feed_bindings:delete',               'Delete feed bindings');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::market_series_asset_classes:write',  'Assign asset classes to market data series');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::market_series_asset_classes:delete', 'Remove asset classes from market data series');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::observation_lineages:write',         'Create and modify observation lineages');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::observation_lineages:delete',        'Delete observation lineages');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::series_classification_rules:write',  'Create and modify series classification rules');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::series_classification_rules:delete', 'Delete series classification rules');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::observations:read',                  'Export every market data series, observation and fixing as ORE files');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::import:write',                       'Import ORE market data and fixings files');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::crm:read',                           'Read cross rates derived from live market data');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::curve_bootstrap:compute',            'Bootstrap a curve and return it without storing it');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::curve_bootstrap:republish',          'Bootstrap a curve and store its output series');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'marketdata::*', 'Full access to all market data operations');

    -- =============================================================================
    -- Analytics Permissions
    -- =============================================================================
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_engine_types:read',   'View pricing engine types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_engine_types:write',  'Create and modify pricing engine types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::pricing_engine_types:delete', 'Delete pricing engine types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_shift_families:read', 'View stress shift families');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_shift_families:write', 'Create and modify stress shift families');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::stress_shift_families:delete', 'Delete stress shift families');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::shift_types:read', 'View shift types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::shift_types:write', 'Create and modify shift types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::shift_types:delete', 'Delete shift types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_collection_kinds:read', 'View today''s market collection kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_collection_kinds:write', 'Create and modify today''s market collection kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'analytics::todays_market_collection_kinds:delete', 'Delete today''s market collection kinds');
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
    -- Storage Component Permissions
    -- =============================================================================

    -- Object storage. One code per operation, and a read is never the same code
    -- as a write. Buckets are the caller's concern: there is no per-bucket code
    -- and no bucket name in this file, so the server holds no list to fall
    -- behind.
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'storage::objects:read',   'Read and list stored objects');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'storage::objects:write',  'Create and replace stored objects');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'storage::objects:delete', 'Delete stored objects');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'storage::*', 'Full access to all storage operations');

    -- =============================================================================
    -- Inbox Component Permissions
    -- =============================================================================

    -- Approval requests and their lookups. Deciding a request of a kind needs
    -- the permission the kind names, not one of these.
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_kinds:read',            'Read and list approval kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_kinds:write',           'Create and update approval kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_kinds:delete',          'Delete approval kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_request_states:read',   'Read and list approval request states');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_request_states:write',  'Create and update approval request states');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_request_states:delete', 'Delete approval request states');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_decision_types:read',   'Read and list approval decision types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_decision_types:write',  'Create and update approval decision types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_decision_types:delete', 'Delete approval decision types');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_requests:read',         'Read and list approval requests');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_requests:write',        'Create and update approval requests');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_requests:delete',       'Delete approval requests');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_decisions:read',        'Read and list approval decisions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_decisions:write',       'Create and update approval decisions');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::approval_decisions:delete',      'Delete approval decisions');

    -- Notifications and their lookups. A person reads their own notifications
    -- through the service, which scopes them to the person, not through these.
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_kinds:read',         'Read and list notification kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_kinds:write',        'Create and update notification kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_kinds:delete',       'Delete notification kinds');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_channels:read',      'Read and list notification channels');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_channels:write',     'Create and update notification channels');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_channels:delete',    'Delete notification channels');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notifications:read',              'Read and list notifications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notifications:write',             'Create and update notifications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notifications:delete',            'Delete notifications');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_arguments:read',     'Read and list notification arguments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_arguments:write',    'Create and update notification arguments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_arguments:delete',   'Delete notification arguments');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_recipients:read',    'Read and list notification recipients');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_recipients:write',   'Create and update notification recipients');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_recipients:delete',  'Delete notification recipients');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_deliveries:read',    'Read and list notification deliveries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_deliveries:write',   'Create and update notification deliveries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_deliveries:delete',  'Delete notification deliveries');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_preferences:read',   'Read and list notification preferences');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_preferences:write',  'Create and update notification preferences');
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::notification_preferences:delete', 'Delete notification preferences');

    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), 'inbox::*', 'Full access to all inbox operations');

    -- =============================================================================
    -- Global Wildcard Permission
    -- =============================================================================

    -- Wildcard permission (superuser)
    PERFORM ores_iam_permissions_upsert_fn(ores_utility_system_tenant_id_fn(), '*', 'Full access to all operations');
END $$;


-- Show summary
select count(*) as total_permissions from ores_iam_permissions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
