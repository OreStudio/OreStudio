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

-- Utility functions (must be first - other components depend on these)
\ir ./utility/create_utility.sql

-- Change control (must be created before entities that reference them)
\ir ./change_control/create_change_control.sql

-- Reference data tables
\ir ./refdata/create_refdata.sql

-- IAM tables
\ir ./iam/create_iam.sql

-- Variability tables
\ir ./variability/create_variability.sql

-- Telemetry tables
\ir ./telemetry/create_telemetry.sql

-- Asset tables
\ir ./assets/create_assets.sql

-- Geo tables
\ir ./geo/create_geo.sql

-- Data Quality tables
\ir ./dq/create_dq.sql

-- Seed functions
\ir ./seed/create_seed.sql
