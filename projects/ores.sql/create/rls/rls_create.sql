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
-- Row-Level Security Policies
-- =============================================================================
-- RLS policies must be created after all tables and functions are defined.
-- This orchestration file includes all component RLS policies.

\ir ../iam/iam_rls_policies_create.sql
\ir ../dq/dq_rls_policies_create.sql
\ir ../refdata/refdata_rls_policies_create.sql
\ir ../assets/assets_rls_policies_create.sql
\ir ../variability/variability_rls_policies_create.sql
\ir ../telemetry/telemetry_rls_policies_create.sql
\ir ../geo/geo_rls_policies_create.sql
