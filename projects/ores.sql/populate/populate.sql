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
 * System Population Script
 *
 * Seeds the database with governance and catalogue data. Foundation layer
 * data (change reasons, IAM, coding schemes, etc.) is already populated
 * by setup_schema.sql before this script runs.
 *
 * Population Layers:
 *
 * 1. Foundation Layer (already in template - not run here)
 *    - Change Control: Categories and reasons for audit trail
 *    - Reference Data Lookup Tables: Rounding types
 *    - Data Governance Framework: Domains, subject areas, authority types, coding schemes
 *    - IAM: Permissions and roles
 *    - System Configuration: Feature flags
 *
 * 2. Data Governance Layer
 *    - Dimensions: Quality classification (origin, nature, treatment)
 *    - Methodologies: Data sourcing and processing methods
 *    - Artefact Types: Types of data artefacts and their mappings
 *
 * 3. Data Catalogues Layer
 *    - Catalogs: Groupings of datasets by source/domain
 *    - Datasets: Data collection definitions with metadata
 *    - Dataset Dependencies: Relationships between datasets
 *    - Artefact Data: Actual reference data in staging tables
 *
 * NOTE: Production tables (countries, currencies, images) are NOT populated
 * here. Use the "Publish Datasets" feature in the Data Librarian window to
 * publish data from DQ artefact tables to production tables.
 *
 * Usage:
 *   psql -U ores_cli_user -d your_database -f populate/populate.sql
 */

-- Suppress noisy output during population
\timing off
\pset tuples_only on

\echo '=== Starting System Population ==='
\echo ''

-- =============================================================================
-- Data Governance Layer
-- =============================================================================

\echo '--- Data Governance Layer ---'
\ir dq/dq_populate.sql

-- =============================================================================
-- Data Catalogues Layer
-- =============================================================================

\echo ''
\echo '--- Data Catalogues Layer ---'
\ir catalogues/catalogues_populate.sql

-- =============================================================================
-- Acme Bank Dataset (self-published DQ artefact; synthetic reference-data
-- tenant used to demo/exercise the full system end to end. Must follow the
-- catalogues layer, since it references the 'OreStudio Code Generation
-- Methodology'. Publish-from-dq wiring lands with the server-side-
-- orchestration follow-up task.)
-- =============================================================================

\echo ''
\echo '--- Acme Bank Dataset ---'
\ir acme/acme_catalog_populate.sql
\ir acme/acme_dataset_populate.sql
\ir acme/acme_lei_entities_artefact_populate.sql
\ir acme/acme_lei_relationships_artefact_populate.sql
\ir acme/acme_uk_business_units_artefact_populate.sql
\ir acme/acme_uk_portfolios_artefact_populate.sql
\ir acme/acme_uk_books_artefact_populate.sql
\ir acme/acme_uk_accounts_artefact_populate.sql
\ir acme/acme_uk_account_contact_informations_artefact_populate.sql
\ir acme/acme_us_business_units_artefact_populate.sql
\ir acme/acme_us_portfolios_artefact_populate.sql
\ir acme/acme_us_books_artefact_populate.sql
\ir acme/acme_us_accounts_artefact_populate.sql
\ir acme/acme_us_account_contact_informations_artefact_populate.sql
\ir acme/acme_hk_business_units_artefact_populate.sql
\ir acme/acme_hk_portfolios_artefact_populate.sql
\ir acme/acme_hk_books_artefact_populate.sql
\ir acme/acme_hk_accounts_artefact_populate.sql
\ir acme/acme_hk_account_contact_informations_artefact_populate.sql

-- =============================================================================
-- Badge Severities Dataset (self-published DQ artefact; must follow the
-- catalogues layer, since it references the 'ORE' catalog. Feeds the
-- publish-from-dq pipeline so any tenant can get its own copy of the
-- badge severities catalogue.)
-- =============================================================================

\echo ''
\echo '--- Badge Severities Dataset ---'
\ir dq/dq_badge_severities_dataset_populate.sql
\ir dq/dq_badge_severities_artefact_populate.sql

-- =============================================================================
-- Badge Definitions Dataset (self-published DQ artefact; must follow the
-- catalogues layer, since it references the 'ORE' catalog. Feeds the
-- publish-from-dq pipeline so any tenant can get its own copy of the
-- badge definitions catalogue.)
-- =============================================================================

\echo ''
\echo '--- Badge Definitions Dataset ---'
\ir dq/dq_badge_definitions_dataset_populate.sql
\ir dq/dq_badge_definitions_artefact_populate.sql

-- =============================================================================
-- Code Domains Dataset (self-published DQ artefact; must follow the
-- catalogues layer, since it references the 'ORE' catalog. Feeds the
-- publish-from-dq pipeline so any tenant can get its own copy of the
-- code domains catalogue.)
-- =============================================================================

\echo ''
\echo '--- Code Domains Dataset ---'
\ir dq/dq_code_domains_dataset_populate.sql
\ir dq/dq_code_domains_artefact_populate.sql

-- =============================================================================
-- Badge Mappings Dataset (self-published DQ artefact; must follow the
-- catalogues layer, since it references the 'ORE' catalog. Feeds the
-- publish-from-dq pipeline so any tenant can get its own copy of the
-- badge mappings catalogue.)
-- =============================================================================

\echo ''
\echo '--- Badge Mappings Dataset ---'
\ir dq/dq_badge_mappings_dataset_populate.sql
\ir dq/dq_badge_mappings_artefact_populate.sql

-- =============================================================================
-- Reporting Layer
-- =============================================================================

\echo ''
\echo '--- Reporting Layer ---'
\ir reporting/reporting_populate.sql

-- =============================================================================
-- Synthetic Layer
-- =============================================================================

\echo ''
\echo '--- Synthetic Layer ---'
\ir synthetic/synthetic_populate.sql

-- =============================================================================
-- Market Data Layer
-- =============================================================================

\echo ''
\echo '--- Market Data Layer ---'
\ir marketdata/marketdata_populate.sql

-- =============================================================================
-- Refdata Layer
-- =============================================================================

\echo ''
\echo '--- Refdata Layer ---'
\ir refdata/refdata_populate.sql

-- =============================================================================
-- FSM Layer (must precede Trading — activity types reference FSM transitions)
-- =============================================================================

\echo ''
\echo '--- FSM Layer ---'
\ir dq/dq_fsm_populate.sql

-- =============================================================================
-- Trading Layer
-- =============================================================================

\echo ''
\echo '--- Trading Layer ---'
\ir trading/populate_trading.sql

-- =============================================================================
-- Compute Layer
-- =============================================================================

\echo ''
\echo '--- Compute Layer ---'
\ir compute/compute_populate.sql
\ir compute/compute_platforms_seed.sql
\ir compute/compute_ore_app_seed.sql

-- =============================================================================
-- Analytics Layer
-- =============================================================================

\echo ''
\echo '--- Analytics Layer ---'
\ir analytics/populate_analytics.sql

\echo ''
\echo '=== System Population Complete ==='

-- Summary - restore normal output format
\pset tuples_only off

\echo ''
\echo '--- Overall Summary ---'

-- Foundation
select 'Foundation: Change Reason Categories' as entity, count(*) as count
from ores_dq_change_reason_categories_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Change Reasons', count(*)
from ores_dq_change_reasons_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Rounding Types', count(*)
from ores_refdata_rounding_types_tbl
union all
select 'Foundation: Data Domains', count(*)
from ores_dq_data_domains_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Subject Areas', count(*)
from ores_dq_subject_areas_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Coding Scheme Authority Types', count(*)
from ores_dq_coding_scheme_authority_types_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Coding Schemes', count(*)
from ores_dq_coding_schemes_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Permissions', count(*)
from ores_iam_permissions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: Roles', count(*)
from ores_iam_roles_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Foundation: System Settings', count(*)
from ores_variability_system_settings_tbl where name like 'system.%' and valid_to = ores_utility_infinity_timestamp_fn()
-- Governance
union all
select 'Governance: Origin Dimensions', count(*)
from ores_dq_origin_dimensions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Governance: Nature Dimensions', count(*)
from ores_dq_nature_dimensions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Governance: Treatment Dimensions', count(*)
from ores_dq_treatment_dimensions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Governance: Methodologies', count(*)
from ores_dq_methodologies_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Governance: Artefact Types', count(*)
from ores_dq_artefact_types_tbl
-- Catalogues
union all
select 'Catalogues: Catalogs', count(*)
from ores_dq_catalogs_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Catalogues: Datasets', count(*)
from ores_dq_datasets_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Catalogues: Dataset Dependencies', count(*)
from ores_dq_dataset_dependencies_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Catalogues: Artefact Images', count(*)
from ores_dq_images_artefact_tbl
union all
select 'Catalogues: Artefact Countries', count(*)
from ores_dq_countries_artefact_tbl
union all
select 'Catalogues: Artefact Currencies', count(*)
from ores_dq_currencies_artefact_tbl
union all
select 'Catalogues: Artefact IP Ranges', count(*)
from ores_dq_ip2country_artefact_tbl
-- Compute
union all
select 'Compute: Apps', count(*)
from ores_compute_apps_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Compute: App Versions', count(*)
from ores_compute_app_versions_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Compute: Platforms', count(*)
from ores_compute_platforms_tbl where valid_to = ores_utility_infinity_timestamp_fn()
-- Analytics
union all
select 'Analytics: Pricing Engine Types', count(*)
from ores_analytics_pricing_engine_types_tbl where valid_to = ores_utility_infinity_timestamp_fn()
-- Workspaces (Live = id 0 is seeded at schema creation, not here)
union all
select 'Workspace: Workspaces', count(*)
from ores_workspaces_tbl
order by entity;
