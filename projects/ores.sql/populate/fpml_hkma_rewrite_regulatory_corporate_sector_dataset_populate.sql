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
 * FpML Hkma Rewrite Regulatory Corporate Sector Dataset Population Script
 *
 * Creates the dataset entry for fpml.hkma_rewrite_regulatory_corporate_sector.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Hkma Rewrite Regulatory Corporate Sector Dataset
-- =============================================================================

\echo '--- FpML Hkma Rewrite Regulatory Corporate Sector Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.hkma_rewrite_regulatory_corporate_sector',
    'FpML Standards',
    'Regulatory',
    'Reference Data',
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Hkma Rewrite Regulatory Corporate Sector',
    'Defines the corporate sector under HKMA (Hong Kong Monetary Authority) Rewrite fields 190 - Nature of Counterparty 1 and 191 - Nature of Counterparty 2.',
    'FPML',
    'Reference data for FpML Hkma Rewrite Regulatory Corporate Sector',
    current_date,
    'FpML Public License 2.0',
    'regulatory_corporate_sectors',
    'refdata_regulatory_corporate_sectors_tbl',
    'dq_populate_regulatory_corporate_sectors'
);
