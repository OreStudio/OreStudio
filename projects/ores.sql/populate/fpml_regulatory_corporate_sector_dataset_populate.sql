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
 * FpML Regulatory Corporate Sector Dataset Population Script
 *
 * Creates the dataset entry for fpml.regulatory_corporate_sector.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Regulatory Corporate Sector Dataset
-- =============================================================================

\echo '--- FpML Regulatory Corporate Sector Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.regulatory_corporate_sector',
    'FpML Standards',
    'Regulatory',
    'Reference Data',
    'FPML_REGULATORY_CORPORATE_SECTOR',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Regulatory Corporate Sector',
    'Specifies Corporate sector as defined by or for regulators including ESMA, CFTC, etc.',
    'FPML',
    'Reference data for FpML Regulatory Corporate Sector',
    current_date,
    'FpML Public License 2.0',
    'regulatory_corporate_sectors',
    'refdata_regulatory_corporate_sectors_tbl',
    'dq_populate_regulatory_corporate_sectors'
);
