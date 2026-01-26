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
 * Foundation Layer Population Script
 *
 * Seeds the database with essential lookup and configuration data required for
 * schema integrity. This data is included in the template database and must be
 * present before application-level data can be inserted.
 *
 * The foundation layer includes:
 * - Change Control: Categories and reasons for audit trail
 * - Reference Data Lookup Tables: Rounding types
 * - Data Governance Framework: Domains, subject areas, authority types, coding schemes
 * - IAM: Permissions and roles for access control
 * - System Configuration: Feature flags for runtime configuration
 *
 * All scripts are idempotent and can be safely re-run.
 *
 * Usage:
 *   psql -U ores -d your_database -f populate/foundation/populate_foundation.sql
 */

-- Suppress noisy output during population
\timing off
\pset tuples_only on

\echo '=== Foundation Layer Population ==='
\echo ''

-- =============================================================================
-- Change Control (must be populated before entities that use change reasons)
-- =============================================================================

\echo '--- Change Control ---'
\ir ../dq/dq_change_reasons_populate.sql

-- =============================================================================
-- Reference Data Lookup Tables
-- =============================================================================

\echo ''
\echo '--- Reference Data Lookup Tables ---'
\ir ../refdata/refdata_rounding_types_populate.sql

-- =============================================================================
-- Data Governance Framework
-- =============================================================================

\echo ''
\echo '--- Data Governance Framework ---'

-- Data domains (must come before subject areas)
\ir ../dq/dq_data_domain_populate.sql

-- Subject areas (depends on data domains)
\ir ../dq/dq_subject_area_populate.sql

-- Coding scheme authority types (must come before coding schemes)
\ir ../dq/dq_coding_scheme_authority_type_populate.sql

-- Coding schemes (depends on subject areas and authority types)
\ir ../dq/dq_coding_scheme_populate.sql

-- =============================================================================
-- IAM (Identity and Access Management)
-- =============================================================================

\echo ''
\echo '--- IAM ---'
\ir ../iam/populate_iam.sql

-- =============================================================================
-- System Configuration (Feature Flags)
-- =============================================================================

\echo ''
\echo '--- System Configuration ---'
\ir ../variability/populate_variability.sql

\echo ''
\echo '=== Foundation Layer Population Complete ==='

-- Summary - restore normal output format
\pset tuples_only off

\echo ''
\echo '--- Foundation Layer Summary ---'

select 'Change Reason Categories' as entity, count(*) as count
from metadata.dq_change_reason_categories_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Change Reasons', count(*)
from metadata.dq_change_reasons_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Rounding Types', count(*)
from production.refdata_rounding_types_tbl
union all
select 'Data Domains', count(*)
from metadata.dq_data_domains_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Subject Areas', count(*)
from metadata.dq_subject_areas_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Coding Scheme Authority Types', count(*)
from metadata.dq_coding_scheme_authority_types_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Coding Schemes', count(*)
from metadata.dq_coding_schemes_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Permissions', count(*)
from production.iam_permissions_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'Roles', count(*)
from production.iam_roles_tbl where valid_to = public.utility_infinity_timestamp_fn()
union all
select 'System Flags', count(*)
from production.variability_feature_flags_tbl where name like 'system.%' and valid_to = public.utility_infinity_timestamp_fn()
order by entity;
