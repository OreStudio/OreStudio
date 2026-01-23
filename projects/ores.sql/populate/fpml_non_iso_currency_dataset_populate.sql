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
 * FpML Non Iso Currency Dataset Population Script
 *
 * Creates the dataset entry for fpml.non_iso_currency.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Non Iso Currency Dataset
-- =============================================================================

\echo '--- FpML Non Iso Currency Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.non_iso_currency',
    'FpML Standards',
    'Currencies',
    'Reference Data',
    'FPML_NON_ISO_CURRENCY',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Non Iso Currency',
    'Includes the currency codes to expand the ISO 4217 currency list, including the offshore and historical currencies.',
    'FPML',
    'Reference data for FpML Non Iso Currency',
    current_date,
    'FpML Public License 2.0',
    'non_iso_currencies',
    'refdata_non_iso_currencies_tbl',
    'dq_populate_non_iso_currencies'
);
