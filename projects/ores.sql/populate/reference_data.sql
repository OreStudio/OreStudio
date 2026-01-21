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
 * Reference Data Population Script
 *
 * Populates production tables with reference data from DQ staging datasets.
 * This is a convenience wrapper for standalone use - the same functionality
 * is included in populate.sql.
 *
 * Prerequisites:
 *   - DQ staging tables must be populated (run populate.sql first, or
 *     run the individual dq_*_artefact_populate.sql scripts)
 *
 * Usage:
 *   psql -U ores -d your_database -f populate/reference_data.sql
 */

-- Suppress noisy output during population
\timing off
\pset tuples_only on

\echo '=== Starting Reference Data Population ==='
\echo ''

-- Populate production tables from DQ staging data
\ir dq_populate_production.sql

\echo ''
\echo '=== Reference Data Population Complete ==='
