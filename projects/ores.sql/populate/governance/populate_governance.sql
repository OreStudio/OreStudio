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
 * Data Governance Layer Population Script
 *
 * Seeds the database with data governance metadata that defines the rules and
 * classifications for how data is organized. This layer sets out the landscape
 * for data quality management.
 *
 * The data governance layer includes:
 * - Dimensions: Quality classification (origin, nature, treatment)
 * - Methodologies: Data sourcing and processing methods
 * - Artefact Types: Types of data artefacts and their mappings
 *
 * All scripts are idempotent and can be safely re-run.
 *
 * Usage:
 *   psql -U ores -d your_database -f populate/governance/populate_governance.sql
 */

-- Suppress noisy output during population
\timing off
\pset tuples_only on

\echo '=== Data Governance Layer Population ==='
\echo ''

-- =============================================================================
-- Dimensions (Origin, Nature, Treatment)
-- =============================================================================

\echo '--- Dimensions ---'
\ir ../dq/dq_origin_dimension_populate.sql
\ir ../dq/dq_nature_dimension_populate.sql
\ir ../dq/dq_treatment_dimension_populate.sql

-- =============================================================================
-- General Methodologies
-- =============================================================================

\echo ''
\echo '--- General Methodologies ---'
\ir ../dq/dq_methodology_populate.sql

-- =============================================================================
-- Artefact Types
-- =============================================================================

\echo ''
\echo '--- Artefact Types ---'
\ir ../dq/dq_artefact_types_populate.sql

-- =============================================================================
-- Dataset Bundles
-- =============================================================================

\echo ''
\echo '--- Dataset Bundles ---'
\ir ./dq_dataset_bundle_populate.sql
\ir ./dq_dataset_bundle_member_populate.sql

\echo ''
\echo '=== Data Governance Layer Population Complete ==='

-- Summary - restore normal output format
\pset tuples_only off

\echo ''
\echo '--- Data Governance Layer Summary ---'

select 'Origin Dimensions' as entity, count(*) as count
from metadata.dq_origin_dimensions_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Nature Dimensions', count(*)
from metadata.dq_nature_dimensions_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Treatment Dimensions', count(*)
from metadata.dq_treatment_dimensions_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Methodologies', count(*)
from metadata.dq_methodologies_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Artefact Types', count(*)
from metadata.dq_artefact_types_tbl
union all
select 'Dataset Bundles', count(*)
from metadata.dq_dataset_bundles_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Dataset Bundle Members', count(*)
from metadata.dq_dataset_bundle_members_tbl where valid_to = public.utility_infinity_timestamp_fn()
order by entity;
