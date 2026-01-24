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
 * FpML Benchmark Rate Dataset Population Script
 *
 * Creates the dataset entry for fpml.benchmark_rate.
 * Source version: 1-4
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Benchmark Rate Dataset
-- =============================================================================

\echo '--- FpML Benchmark Rate Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.benchmark_rate',
    'FpML Standards',
    'Market Data',
    'Reference Data',
    'FPML_BENCHMARK_RATE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Benchmark Rate',
    'FpML Benchmark rates',
    'FPML',
    'Reference data for FpML Benchmark Rate (version 1-4)',
    '2025-09-26'::date,
    'FpML Public License 2.0',
    'benchmark_rates',
    'refdata_benchmark_rates_tbl',
    'dq_populate_benchmark_rates'
);
