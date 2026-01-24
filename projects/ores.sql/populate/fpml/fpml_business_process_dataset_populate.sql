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
 * FpML Business Process Dataset Population Script
 *
 * Creates the dataset entry for fpml.business_process.
 * Source version: 1-0
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Business Process Dataset
-- =============================================================================

\echo '--- FpML Business Process Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.business_process',
    'FpML Standards',
    'Trading',
    'Reference Data',
    'FPML_BUSINESS_PROCESS',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Business Process',
    'Contains a code representing the type of business process a message (e.g. a status request) applies to.',
    'FPML',
    'Reference data for FpML Business Process (version 1-0)',
    '2011-09-29'::date,
    'FpML Public License 2.0',
    'business_processes',
    'refdata_business_processes_tbl',
    'dq_populate_business_processes'
);
