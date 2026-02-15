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
 * Test Data Catalog Population Script
 *
 * Creates the Test Data catalog for manufactured DQ datasets
 * used to exercise business unit, portfolio, and book tables.
 * This script is idempotent.
 */

-- =============================================================================
-- Test Data Catalog
-- =============================================================================

\echo '--- Test Data Catalog ---'

select ores_dq_catalogs_upsert_fn(ores_iam_system_tenant_id_fn(),
    'Test Data',
    'Manufactured test data for exercising reference data tables including business units, portfolios, and books.',
    'Development Team'
);
