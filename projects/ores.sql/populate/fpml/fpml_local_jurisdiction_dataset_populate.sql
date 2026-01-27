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
 * FpML Local Jurisdiction Dataset Population Script
 *
 * Creates the dataset entry for fpml.local_jurisdiction.
 * Source version: 1-1
 * This must be run before populating the artefact table.
 */

set schema 'metadata';

-- =============================================================================
-- FpML Local Jurisdiction Dataset
-- =============================================================================

\echo '--- FpML Local Jurisdiction Dataset ---'

select metadata.upsert_dq_datasets(
    'fpml.local_jurisdiction',
    'FpML Standards',
    'Regulatory',
    'Reference Data',
    'FPML_LOCAL_JURISDICTION',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Local Jurisdiction',
    'This overrides the countryScheme. Specifies the Local Jurisdiction that applies to a Transaction, for example for the purposes of defining which Local Taxes will apply.',
    'FPML',
    'Reference data for FpML Local Jurisdiction (version 1-1)',
    '2013-01-15'::date,
    'FpML Public License 2.0',
    'local_jurisdictions'
);
