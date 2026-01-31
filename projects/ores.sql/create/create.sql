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

-- =============================================================================
-- 1. Utility Functions (shared utilities used by all tables)
-- =============================================================================
\ir ./utility/create_utility.sql

-- =============================================================================
-- 2. Data Governance Tables (must be created before operational tables - no external deps)
-- =============================================================================
\ir ./dq/create_dq.sql

-- =============================================================================
-- 3. Operational Tables (depend on data governance tables for FK validation)
-- =============================================================================
\ir ./refdata/create_refdata.sql
\ir ./iam/create_iam.sql
\ir ./variability/create_variability.sql
\ir ./telemetry/create_telemetry.sql
\ir ./assets/create_assets.sql
\ir ./geo/create_geo.sql

-- =============================================================================
-- 4. Seed Functions (depend on data governance and operational tables)
-- =============================================================================
\ir ./seed/create_seed.sql

-- =============================================================================
-- 5. Summary Functions (depend on all tables)
-- =============================================================================
\ir ./utility/utility_summary_functions_create.sql
