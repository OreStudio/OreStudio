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
 * Payment Frequency Types Population Script
 *
 * Seeds the database with coupon/payment frequency codes used in instrument legs.
 * Values sourced from ORE ore_types.xsd Frequency enumeration.
 * This script is idempotent.
 */

\echo '--- Payment Frequency Types ---'

insert into ores_trading_payment_frequency_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Annual',      ores_iam_system_tenant_id_fn(), 0, 'Annual (once per year)',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types'),
    ('Semiannual',  ores_iam_system_tenant_id_fn(), 0, 'Semiannual (twice per year)',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types'),
    ('EveryFourthMonth', ores_iam_system_tenant_id_fn(), 0, 'Every fourth month (3 per year)',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types'),
    ('Quarterly',   ores_iam_system_tenant_id_fn(), 0, 'Quarterly (4 per year)',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types'),
    ('Bimonthly',   ores_iam_system_tenant_id_fn(), 0, 'Bimonthly (every 2 months)',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types'),
    ('Monthly',     ores_iam_system_tenant_id_fn(), 0, 'Monthly (12 per year)',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types'),
    ('EveryFourthWeek', ores_iam_system_tenant_id_fn(), 0, 'Every fourth week',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types'),
    ('Biweekly',    ores_iam_system_tenant_id_fn(), 0, 'Biweekly (every 2 weeks)',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types'),
    ('Weekly',      ores_iam_system_tenant_id_fn(), 0, 'Weekly',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types'),
    ('Daily',       ores_iam_system_tenant_id_fn(), 0, 'Daily',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types'),
    ('Once',        ores_iam_system_tenant_id_fn(), 0, 'Single payment (bullet)',
     'ores_trading_service', 'system.initial_load', 'Seed payment frequency types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Payment Frequency Types' as entity, count(*) as count
from ores_trading_payment_frequency_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
