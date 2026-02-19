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
\ir ./utility/utility_create.sql

-- =============================================================================
-- 2. Data Governance Tables (must be created before operational tables - no external deps)
-- =============================================================================
\ir ./dq/dq_create.sql

-- =============================================================================
-- 3. Operational Tables (depend on data governance tables for FK validation)
-- =============================================================================
\ir ./fsm/fsm_create.sql
\ir ./trade/trade_create.sql
\ir ./refdata/refdata_create.sql
\ir ./iam/iam_create.sql
\ir ./variability/variability_create.sql
\ir ./telemetry/telemetry_create.sql
\ir ./assets/assets_create.sql
\ir ./geo/geo_create.sql

-- =============================================================================
-- 4. Row-Level Security Policies (depend on all tables and IAM functions)
-- =============================================================================
\ir ./rls/rls_create.sql

-- =============================================================================
-- 5. Seed Functions (depend on data governance and operational tables)
-- =============================================================================
\ir ./seed/seed_create.sql

-- =============================================================================
-- 6. Summary Functions (depend on all tables)
-- =============================================================================
\ir ./utility/utility_summary_functions_create.sql
