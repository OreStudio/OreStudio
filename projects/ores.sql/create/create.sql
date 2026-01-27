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
-- 1. Public Schema (shared utilities)
-- =============================================================================
\ir ./utility/create_utility.sql

-- =============================================================================
-- 2. Metadata Schema (must be created before production - no external deps)
-- =============================================================================
\ir ./change_control/create_change_control.sql
\ir ./dq/create_dq.sql

-- =============================================================================
-- 3. Production Schema (depends on metadata for FK validation)
-- =============================================================================
\ir ./refdata/create_refdata.sql
\ir ./iam/create_iam.sql
\ir ./variability/create_variability.sql
\ir ./telemetry/create_telemetry.sql
\ir ./assets/create_assets.sql
\ir ./geo/create_geo.sql

-- =============================================================================
-- 4. Seed Functions (public schema, depends on metadata and production tables)
-- =============================================================================
\ir ./seed/create_seed.sql

-- =============================================================================
-- 5. Summary Functions (public schema, depends on all tables)
-- =============================================================================
\ir ./utility/utility_summary_functions_create.sql
