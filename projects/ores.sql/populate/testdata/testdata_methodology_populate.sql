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
 * Test Data Methodology Population Script
 *
 * Creates the methodology for manufactured test data.
 * This script is idempotent.
 */

-- =============================================================================
-- Test Data Methodology
-- =============================================================================

\echo '--- Test Data Methodology ---'

select ores_dq_methodologies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'OreStudio Test Data Generation',
    'Manufactured test data generated for exercising reference data entities.',
    null,
    'Test data is manually crafted to create realistic hierarchical structures
for business units, portfolios, and books. Data represents a fictitious
global markets organisation with regional trading desks.'
);
