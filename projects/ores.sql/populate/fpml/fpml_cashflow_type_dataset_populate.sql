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
 * FpML Cashflow Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.cashflow_type.
 * Source version: 2-0
 * This must be run before populating the artefact table.
 */

set schema 'metadata';

-- =============================================================================
-- FpML Cashflow Type Dataset
-- =============================================================================

\echo '--- FpML Cashflow Type Dataset ---'

select public.upsert_dq_datasets(
    'fpml.cashflow_type',
    'FpML Standards',
    'Trading',
    'Reference Data',
    'FPML_CASHFLOW_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Cashflow Type',
    'The type of cash flows associated with OTC derivatives contracts and their lifecycle events.',
    'FPML',
    'Reference data for FpML Cashflow Type (version 2-0)',
    current_date,
    'FpML Public License 2.0',
    'cashflow_types'
);
