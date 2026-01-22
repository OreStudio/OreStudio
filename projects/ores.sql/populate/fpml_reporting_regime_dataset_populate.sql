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
 * FpML Reporting Regime Dataset Population Script
 *
 * Creates the dataset entry for fpml.reporting_regime.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Reporting Regime Dataset
-- =============================================================================

\echo '--- FpML Reporting Regime Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.reporting_regime',
    'FpML Standards',
    'Regulatory',
    'Reference Data',
    'FPML_REPORTING_REGIME',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Reporting Regime',
    'Contains a code representing a reporting regime under which this transaction may be reported.',
    'FPML',
    'Reference data for FpML Reporting Regime',
    current_date,
    'FpML Public License 2.0',
    'reporting_regimes',
    'refdata_reporting_regimes_tbl',
    'dq_populate_reporting_regimes'
);
