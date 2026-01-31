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
 * FpML Asset Class Dataset Population Script
 *
 * Creates the dataset entry for fpml.asset_class.
 * Source version: 2-1
 * This must be run before populating the artefact table.
 */


-- =============================================================================
-- FpML Asset Class Dataset
-- =============================================================================

\echo '--- FpML Asset Class Dataset ---'

select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'fpml.asset_class',
    'FpML Standards',
    'Market Data',
    'Reference Data',
    'FPML_ASSET_CLASS',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Asset Class',
    'Defines a simple asset class categorization. Used for classification of the risk class of the trade.',
    'FPML',
    'Reference data for FpML Asset Class (version 2-1)',
    '2019-10-01'::date,
    'FpML Public License 2.0',
    'asset_classes'
);
