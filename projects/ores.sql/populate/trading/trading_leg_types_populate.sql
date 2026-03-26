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
 * Leg Types Population Script
 *
 * Seeds the database with swap/instrument leg type codes used in leg definitions.
 * Values sourced from ORE instruments.xsd LegType enumeration.
 * This script is idempotent.
 */

\echo '--- Leg Types ---'

insert into ores_trading_leg_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Fixed',       ores_iam_system_tenant_id_fn(), 0, 'Fixed rate leg',
     'ores_trading_service', 'system.initial_load', 'Seed leg types'),
    ('Floating',    ores_iam_system_tenant_id_fn(), 0, 'Floating rate leg (IBOR or RFR)',
     'ores_trading_service', 'system.initial_load', 'Seed leg types'),
    ('OIS',         ores_iam_system_tenant_id_fn(), 0, 'Overnight Index Swap leg',
     'ores_trading_service', 'system.initial_load', 'Seed leg types'),
    ('CMS',         ores_iam_system_tenant_id_fn(), 0, 'Constant Maturity Swap leg',
     'ores_trading_service', 'system.initial_load', 'Seed leg types'),
    ('CMSSpread',   ores_iam_system_tenant_id_fn(), 0, 'CMS spread leg',
     'ores_trading_service', 'system.initial_load', 'Seed leg types'),
    ('DigitalCMS',  ores_iam_system_tenant_id_fn(), 0, 'Digital CMS leg',
     'ores_trading_service', 'system.initial_load', 'Seed leg types'),
    ('CPI',         ores_iam_system_tenant_id_fn(), 0, 'Consumer Price Index (inflation) leg',
     'ores_trading_service', 'system.initial_load', 'Seed leg types'),
    ('YoY',         ores_iam_system_tenant_id_fn(), 0, 'Year-on-Year inflation leg',
     'ores_trading_service', 'system.initial_load', 'Seed leg types'),
    ('ZeroCoupon',  ores_iam_system_tenant_id_fn(), 0, 'Zero coupon inflation leg',
     'ores_trading_service', 'system.initial_load', 'Seed leg types'),
    ('AssetSwap',   ores_iam_system_tenant_id_fn(), 0, 'Asset swap leg',
     'ores_trading_service', 'system.initial_load', 'Seed leg types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Leg Types' as entity, count(*) as count
from ores_trading_leg_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
