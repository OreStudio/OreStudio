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
 * FpML Asset Measure Dataset Population Script
 *
 * Creates the dataset entry for fpml.asset_measure.
 * Source version: 5-9
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Asset Measure Dataset
-- =============================================================================

\echo '--- FpML Asset Measure Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.asset_measure',
    'FpML Standards',
    'Market Data',
    'Reference Data',
    'FPML_ASSET_MEASURE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Asset Measure',
    'The type of measure about an asset. Used for escribing valuation, sensitivity, and risk measures.',
    'FPML',
    'Reference data for FpML Asset Measure (version 5-9)',
    '2024-03-14'::date,
    'FpML Public License 2.0',
    'asset_measures',
    'refdata_asset_measures_tbl',
    'dq_populate_asset_measures'
);
