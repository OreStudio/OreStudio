/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * IAM (Identity and Access Management) Master Include File
 *
 * Includes all IAM SQL files in the correct dependency order.
 */

-- =============================================================================
-- Account Types (reference data, must come early)
-- =============================================================================

\echo '--- IAM Account Types ---'
\ir iam_account_types_populate.sql

-- =============================================================================
-- Permissions (must come before roles)
-- =============================================================================

\echo '--- IAM Permissions ---'
\ir iam_permissions_populate.sql

-- =============================================================================
-- Roles and Role-Permission Assignments
-- =============================================================================

\echo '--- IAM Roles ---'
\ir iam_roles_populate.sql

-- =============================================================================
-- Service Accounts (system service accounts for processes)
-- =============================================================================

\echo '--- IAM Service Accounts ---'
\ir iam_service_accounts_populate.sql
