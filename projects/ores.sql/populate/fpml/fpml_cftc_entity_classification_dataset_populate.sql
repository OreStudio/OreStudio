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
 * FpML Cftc Entity Classification Dataset Population Script
 *
 * Creates the dataset entry for fpml.cftc_entity_classification.
 * Source version: 1-0
 * This must be run before populating the artefact table.
 */

set schema 'metadata';

-- =============================================================================
-- FpML Cftc Entity Classification Dataset
-- =============================================================================

\echo '--- FpML Cftc Entity Classification Dataset ---'

select metadata.upsert_dq_datasets(
    'fpml.cftc_entity_classification',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Cftc Entity Classification',
    'Financial Entity Indicator as defined by the CFTC.',
    'FPML',
    'Reference data for FpML Cftc Entity Classification (version 1-0)',
    '2022-11-18'::date,
    'FpML Public License 2.0',
    'entity_classifications'
);
