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
 * Currency Calendars Population Script
 *
 * Seeds real currency-to-calendar mappings for the major currencies
 * already present in seed/synthetic data (mirroring
 * generate_currency_calendars() in currency_calendar_generator.cpp) --
 * real reference data, not fictional test data. Replaces the values
 * previously seeded directly on currency.holiday_calendar.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Currency Calendars ---'

insert into ores_refdata_currency_calendars_tbl (
    tenant_id, currency_iso_code, version, calendar_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'USD', 0, 'UnitedStates.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'CAD', 0, 'Canada.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'MXN', 0, 'Mexico',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'BRL', 0, 'Brazil',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR', 0, 'TARGET',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'GBP', 0, 'UnitedKingdom.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'CHF', 0, 'Switzerland',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'NOK', 0, 'Norway',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'SEK', 0, 'Sweden',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'DKK', 0, 'Denmark',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'JPY', 0, 'Japan',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'CNY', 0, 'China.SSE',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'HKD', 0, 'HongKong',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'KRW', 0, 'SouthKorea.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'SGD', 0, 'Singapore',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'INR', 0, 'India',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'AUD', 0, 'Australia',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'NZD', 0, 'NewZealand',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'ZAR', 0, 'SouthAfrica',
     current_user, current_user, 'system.initial_load', 'Currency spot/settlement calendar reference data')
on conflict (tenant_id, currency_iso_code, calendar_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_currency_calendars' as entity, count(*) as count
from ores_refdata_currency_calendars_tbl;
