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
-- 1. Production Schema (has FKs to metadata, must be dropped first)
-- =============================================================================
\ir ./geo/drop_geo.sql
\ir ./assets/drop_assets.sql
\ir ./telemetry/drop_telemetry.sql
\ir ./variability/drop_variability.sql
\ir ./iam/drop_iam.sql
\ir ./refdata/drop_refdata.sql

-- =============================================================================
-- 2. Metadata Schema (dropped after production)
-- =============================================================================
\ir ./dq/drop_dq.sql
\ir ./change_control/drop_change_control.sql

-- =============================================================================
-- 3. Public Schema (utilities dropped last)
-- =============================================================================
\ir ./seed/drop_seed.sql
\ir ./utility/drop_utility.sql
