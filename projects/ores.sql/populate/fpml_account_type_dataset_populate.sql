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
 * FpML Account Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.account_type.
 * Source version: 1-1
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Account Type Dataset
-- =============================================================================

\echo '--- FpML Account Type Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.account_type',
    'FpML Standards',
    'Trading',
    'Reference Data',
    'FPML_ACCOUNT_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Account Type',
    'Contains a code representing the type of an account, for example in a clearing or exchange model.',
    'FPML',
    'Reference data for FpML Account Type (version 1-1)',
    '2016-06-13'::date,
    'FpML Public License 2.0',
    'account_types',
    'refdata_account_types_tbl',
    'dq_populate_account_types'
);
