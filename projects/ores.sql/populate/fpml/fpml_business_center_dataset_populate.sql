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
 * FpML Business Center Dataset Population Script
 *
 * Creates the dataset entry for fpml.business_center.
 * Source version: 9-4
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Business Center Dataset
-- =============================================================================

\echo '--- FpML Business Center Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.business_center',
    'FpML Standards',
    'Trading',
    'Reference Data',
    'FPML_BUSINESS_CENTER',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Business Center',
    'The coding-scheme accepts a 4 character code of the real geographical business calendar location or FpML format of the rate publication calendar. While the 4 character codes of the business calendar location are implicitly locatable and used for identifying a bad business day for the purpose of payment and rate calculation day adjustments, the rate publication calendar codes are used in the context of the fixing day offsets.',
    'FPML',
    'Reference data for FpML Business Center (version 9-4)',
    '2025-04-25'::date,
    'FpML Public License 2.0',
    'business_centres',
    'refdata_business_centres_tbl',
    'dq_populate_business_centres'
);
