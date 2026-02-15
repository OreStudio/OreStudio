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
 * Test Data Dataset Population Script
 *
 * Registers the datasets for business units, portfolios, and books.
 * This script is idempotent.
 */

-- =============================================================================
-- Test Data Datasets
-- =============================================================================

\echo '--- Test Data Datasets ---'

-- Business Units
select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'testdata.business_units',
    'Test Data',
    'Organisation',
    'Reference Data',
    'NONE',
    'Primary',
    'Synthetic',
    'Raw',
    'OreStudio Test Data Generation',
    'Test Business Units',
    'Manufactured business unit hierarchy for a fictitious global markets organisation.',
    'ORESTUDIO',
    'Test data for exercising business unit tables',
    current_date,
    'Internal Use Only',
    'business_units'
);

-- Portfolios
select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'testdata.portfolios',
    'Test Data',
    'Organisation',
    'Reference Data',
    'NONE',
    'Primary',
    'Synthetic',
    'Raw',
    'OreStudio Test Data Generation',
    'Test Portfolios',
    'Manufactured portfolio tree for a fictitious global markets organisation.',
    'ORESTUDIO',
    'Test data for exercising portfolio tables',
    current_date,
    'Internal Use Only',
    'portfolios'
);

-- Books
select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'testdata.books',
    'Test Data',
    'Organisation',
    'Reference Data',
    'NONE',
    'Primary',
    'Synthetic',
    'Raw',
    'OreStudio Test Data Generation',
    'Test Books',
    'Manufactured trading books as leaf nodes of the portfolio tree.',
    'ORESTUDIO',
    'Test data for exercising book tables',
    current_date,
    'Internal Use Only',
    'books'
);
