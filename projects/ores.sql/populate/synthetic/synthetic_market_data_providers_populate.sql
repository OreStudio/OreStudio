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
 * Market Data Providers Population Script
 *
 * Populates the simulated market data providers a synthetic collection can
 * stand for: the main data vendors, exchanges and brokers, and the generic
 * provider for a system collection that stands for none in particular. Every
 * provider is simulated; its feeds' source names start synthetic.<code>.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Market Data Providers ---'

insert into ores_synthetic_market_data_providers_tbl (
    tenant_id, code, version, name, kind, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'generic', 0, 'Generic', 'generic',
     'A provider that stands for no vendor or venue in particular, for system feeds that simulate the market as a whole.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of market data providers'),
    (ores_utility_system_tenant_id_fn(), 'bloomberg', 0, 'Bloomberg', 'data_vendor',
     'Data vendor publishing quotes across asset classes: FX, rates, credit, equities and commodities.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of market data providers'),
    (ores_utility_system_tenant_id_fn(), 'refinitiv', 0, 'Refinitiv (LSEG)', 'data_vendor',
     'Data vendor publishing quotes across asset classes, including FX spot and the WM/Refinitiv benchmark rates.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of market data providers'),
    (ores_utility_system_tenant_id_fn(), 'ice', 0, 'ICE Data Services', 'data_vendor',
     'Data vendor publishing evaluated prices, reference data and index data, with the ICE exchanges behind it.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of market data providers'),
    (ores_utility_system_tenant_id_fn(), 'cme', 0, 'CME Group', 'exchange',
     'Exchange group listing interest rate, FX, equity index and commodity futures and options.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of market data providers'),
    (ores_utility_system_tenant_id_fn(), 'lse', 0, 'London Stock Exchange', 'exchange',
     'Exchange listing equities, exchange traded funds and bonds.',
     6, current_user, current_user, 'system.initial_load', 'Initial population of market data providers'),
    (ores_utility_system_tenant_id_fn(), 'eurex', 0, 'Eurex', 'exchange',
     'Exchange listing European interest rate, equity index and equity derivatives.',
     7, current_user, current_user, 'system.initial_load', 'Initial population of market data providers'),
    (ores_utility_system_tenant_id_fn(), 'tradeweb', 0, 'Tradeweb', 'broker',
     'Electronic trading venue for rates, credit and money markets, publishing dealer composite prices.',
     8, current_user, current_user, 'system.initial_load', 'Initial population of market data providers')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'synthetic_market_data_providers' as entity, count(*) as count
from ores_synthetic_market_data_providers_tbl;
