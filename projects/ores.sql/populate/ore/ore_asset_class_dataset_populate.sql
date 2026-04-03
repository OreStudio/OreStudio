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

-- =============================================================================
-- ORE Asset Class Dataset
-- =============================================================================

\echo '--- ORE Asset Class Dataset ---'

select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'ore.asset_class',
    'ORE',
    'Market Data',
    'Reference Data',
    'ORE_ASSET_CLASS',
    'Primary',
    'Actual',
    'Raw',
    'ORE Internal',
    'ORE Asset Class',
    'Open Risk Engine asset class codes for classifying market data series and trades. The eight codes cover all ORE-supported asset classes: FX, Rates, Credit, Equity, Commodity, Inflation, Bond, and Cross Asset.',
    'ORE',
    'Reference data for ORE Asset Class (version 1.0)',
    '2026-01-01'::date,
    'Apache License 2.0',
    'asset_classes'
);
