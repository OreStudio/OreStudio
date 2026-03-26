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
 * Day Count Fraction Types Population Script
 *
 * Seeds the database with ORE day count fraction convention codes.
 * Values sourced from ORE ore_types.xsd DayCounter enumeration.
 * This script is idempotent.
 */

\echo '--- Day Count Fraction Types ---'

insert into ores_trading_day_count_fraction_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('A360',         ores_iam_system_tenant_id_fn(), 0, 'Actual/360',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('A365F',        ores_iam_system_tenant_id_fn(), 0, 'Actual/365 (Fixed)',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('A365',         ores_iam_system_tenant_id_fn(), 0, 'Actual/365',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('ActAct(ISDA)', ores_iam_system_tenant_id_fn(), 0, 'Actual/Actual (ISDA)',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('ActAct(ISMA)', ores_iam_system_tenant_id_fn(), 0, 'Actual/Actual (ISMA)',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('ActAct(AFB)',  ores_iam_system_tenant_id_fn(), 0, 'Actual/Actual (AFB)',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('30/360',       ores_iam_system_tenant_id_fn(), 0, '30/360 (Bond Basis)',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('30E/360',      ores_iam_system_tenant_id_fn(), 0, '30E/360 (Eurobond Basis)',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('30E/360(ISDA)',ores_iam_system_tenant_id_fn(), 0, '30E/360 (ISDA)',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('Business252',  ores_iam_system_tenant_id_fn(), 0, 'Business/252 (Brazilian convention)',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('1/1',          ores_iam_system_tenant_id_fn(), 0, '1/1 (unit day count)',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types'),
    ('Simple',       ores_iam_system_tenant_id_fn(), 0, 'Simple day count',
     'ores_trading_service', 'system.initial_load', 'Seed day count fraction types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Day Count Fraction Types' as entity, count(*) as count
from ores_trading_day_count_fraction_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
