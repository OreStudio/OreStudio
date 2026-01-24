/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * IAM (Identity and Access Management) Master Include File
 *
 * Includes all IAM SQL files in the correct dependency order.
 */

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
