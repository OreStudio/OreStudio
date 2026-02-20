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
-- 1. Row-Level Security Policies (must be dropped before tables)
-- =============================================================================
\ir ./rls/rls_drop.sql

-- =============================================================================
-- 2. Operational Tables (have FKs to data governance tables, must be dropped first)
-- =============================================================================
\ir ./geo/geo_drop.sql
\ir ./assets/assets_drop.sql
\ir ./telemetry/telemetry_drop.sql
\ir ./variability/variability_drop.sql
\ir ./iam/iam_drop.sql
\ir ./trading/drop_trading.sql
\ir ./refdata/refdata_drop.sql

-- =============================================================================
-- 3. Data Governance Tables (dropped after operational tables)
-- =============================================================================
\ir ./dq/dq_drop.sql

-- =============================================================================
-- 4. Utility Functions (dropped last)
-- =============================================================================
\ir ./seed/seed_drop.sql
\ir ./utility/utility_drop.sql
