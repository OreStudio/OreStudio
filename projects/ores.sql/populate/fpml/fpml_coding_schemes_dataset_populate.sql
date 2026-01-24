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

/**
 * FpML Coding Schemes Dataset Population Script
 *
 * Creates the dataset entry for fpml.coding_schemes.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Coding Schemes Dataset
-- =============================================================================

\echo '--- FpML Coding Schemes Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.coding_schemes',
    'FpML Standards',
    'General',
    'Reference Data',
    'NONE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Coding Schemes',
    'FpML coding scheme definitions for account types, asset classes, business centers, and other reference data.',
    'FPML',
    'Coding scheme metadata for FpML standards',
    current_date,
    'FpML License',
    'coding_schemes',
    'dq_coding_schemes_tbl',
    'dq_populate_coding_schemes'
);
