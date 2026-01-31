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
 * Data Catalogues Layer Population Script
 *
 * Seeds the database with catalogued reference data. This layer contains the
 * actual data organized according to the governance rules, awaiting publication
 * to production tables.
 *
 * The data catalogues layer includes:
 * - Catalogs: Groupings of datasets by source/domain
 * - Datasets: Data collection definitions with metadata
 * - Dataset Dependencies: Relationships between datasets
 * - Artefact Data: Actual reference data in staging tables
 *
 * Data sources:
 * - Flag Icons: Visual assets for countries
 * - ISO Standards: Countries, currencies (ISO 3166, ISO 4217)
 * - Solvaris: Extended currency metadata
 * - IP to Country: IP geolocation mappings
 * - FPML: Financial products markup language reference data
 * - Cryptocurrency: Digital currency reference data
 *
 * All scripts are idempotent and can be safely re-run.
 *
 * Usage:
 *   psql -U ores_cli_user -d your_database -f populate/catalogues/populate_catalogues.sql
 */

-- Suppress noisy output during population
\timing off
\pset tuples_only on

\echo '=== Data Catalogues Layer Population ==='
\echo ''

-- =============================================================================
-- Flag Icons (Visual Assets catalog, datasets, and images)
-- Must come before ISO to provide flag images for countries
-- =============================================================================

\echo '--- Flag Icons ---'
\ir ../flags/populate_flags.sql

-- =============================================================================
-- ISO Standards (catalog, datasets, countries, currencies)
-- =============================================================================

\echo ''
\echo '--- ISO Standards ---'
\ir ../iso/populate_iso.sql

-- =============================================================================
-- Solvaris (extended currency metadata)
-- =============================================================================

\echo ''
\echo '--- Solvaris ---'
\ir ../solvaris/populate_solvaris.sql

-- =============================================================================
-- IP to Country (IP geolocation mappings)
-- =============================================================================

\echo ''
\echo '--- IP to Country ---'
\ir ../ip2country/populate_ip2country.sql

-- =============================================================================
-- FPML (Financial Products Markup Language reference data)
-- =============================================================================

\echo ''
\echo '--- FPML ---'
\ir ../fpml/populate_fpml.sql

-- =============================================================================
-- Cryptocurrency (digital currency reference data)
-- =============================================================================

\echo ''
\echo '--- Cryptocurrency ---'
\ir ../crypto/populate_crypto.sql

\echo ''
\echo '=== Data Catalogues Layer Population Complete ==='

-- Summary - restore normal output format
\pset tuples_only off

\echo ''
\echo '--- Data Catalogues Layer Summary ---'

select 'Catalogs' as entity, count(*) as count
from ores_dq_catalogs_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Datasets', count(*)
from ores_dq_datasets_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Dataset Dependencies', count(*)
from ores_dq_dataset_dependencies_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'Artefact: Images', count(*)
from ores_dq_images_artefact_tbl
union all
select 'Artefact: Countries', count(*)
from ores_dq_countries_artefact_tbl
union all
select 'Artefact: Currencies', count(*)
from ores_dq_currencies_artefact_tbl
union all
select 'Artefact: IP Ranges', count(*)
from ores_dq_ip2country_artefact_tbl
union all
select 'Artefact: Account Types', count(*)
from ores_dq_account_types_artefact_tbl
union all
select 'Artefact: Asset Classes', count(*)
from ores_dq_asset_classes_artefact_tbl
union all
select 'Artefact: Asset Measures', count(*)
from ores_dq_asset_measures_artefact_tbl
union all
select 'Artefact: Benchmark Rates', count(*)
from ores_dq_benchmark_rates_artefact_tbl
union all
select 'Artefact: Business Centres', count(*)
from ores_dq_business_centres_artefact_tbl
union all
select 'Artefact: Business Processes', count(*)
from ores_dq_business_processes_artefact_tbl
union all
select 'Artefact: Cashflow Types', count(*)
from ores_dq_cashflow_types_artefact_tbl
union all
select 'Artefact: Entity Classifications', count(*)
from ores_dq_entity_classifications_artefact_tbl
union all
select 'Artefact: Local Jurisdictions', count(*)
from ores_dq_local_jurisdictions_artefact_tbl
union all
select 'Artefact: Party Relationships', count(*)
from ores_dq_party_relationships_artefact_tbl
union all
select 'Artefact: Party Roles', count(*)
from ores_dq_party_roles_artefact_tbl
union all
select 'Artefact: Person Roles', count(*)
from ores_dq_person_roles_artefact_tbl
union all
select 'Artefact: Regulatory Corporate Sectors', count(*)
from ores_dq_regulatory_corporate_sectors_artefact_tbl
union all
select 'Artefact: Reporting Regimes', count(*)
from ores_dq_reporting_regimes_artefact_tbl
union all
select 'Artefact: Supervisory Bodies', count(*)
from ores_dq_supervisory_bodies_artefact_tbl
order by entity;
