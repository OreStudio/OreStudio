/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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

set schema 'ores';

-- =============================================================================
-- Batch Execution of solvaris Data Quality Artefacts
-- =============================================================================

\echo '--- Starting Batch Execution of solvaris Data Quality Artefacts ---'

-- Catalogs data for Slovaris
\ir ./solvaris_catalog_populate.sql
-- Methodologies data for Slovaris
\ir ./solvaris_methodology_populate.sql
-- Datasets data for Slovaris
\ir ./solvaris_dataset_populate.sql
-- Dataset dependencies for Slovaris
\ir ./solvaris_dataset_dependency_populate.sql
-- Tags data for Slovaris
\ir ./solvaris_tag_populate.sql
-- Flags data for Slovaris
\ir ./solvaris_flag_populate.sql
-- Currencies data for Slovaris
\ir ./solvaris_currency_populate.sql
-- Countries data for Slovaris
\ir ./solvaris_country_populate.sql

\echo ''
\echo '--- Batch Execution Completed Successfully ---'