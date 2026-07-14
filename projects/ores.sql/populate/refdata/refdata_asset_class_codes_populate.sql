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
 * Asset Class Codes Population Script
 *
 * Populates the top-level asset class classification codes, mirroring
 * ores::marketdata::domain::asset_class.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Asset Class Codes ---'

insert into ores_refdata_asset_class_codes_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'fx', 0, 'FX',
     'Foreign exchange rates, forwards and vol surfaces.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'rates', 0, 'Rates',
     'Interest rate curves, vol surfaces and spreads.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'credit', 0, 'Credit',
     'Credit spreads, hazard rates and recovery rates.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'equity', 0, 'Equity',
     'Equity spot prices, forwards, dividends and vol surfaces.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'commodity', 0, 'Commodity',
     'Commodity spot prices, forwards and vol surfaces.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'inflation', 0, 'Inflation',
     'Inflation swap rates, cap/floor vols and seasonality.',
     6, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'bond', 0, 'Bond',
     'Bond prices and yield spreads.',
     7, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes'),
    (ores_utility_system_tenant_id_fn(), 'cross_asset', 0, 'Cross Asset',
     'Cross-asset correlations.',
     8, current_user, current_user, 'system.initial_load', 'Initial population of asset class codes')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_asset_class_codes' as entity, count(*) as count
from ores_refdata_asset_class_codes_tbl;
