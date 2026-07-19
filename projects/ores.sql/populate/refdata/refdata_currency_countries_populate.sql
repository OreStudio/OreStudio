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
 * Currency Countries Population Script
 *
 * Seeds real currency-to-issuing-country mappings for the major
 * currencies already present in seed/synthetic data (mirroring
 * generate_currency_countries() in currency_country_generator.cpp) --
 * real reference data, not fictional test data. EUR spans multiple
 * countries; every other currency here maps to exactly one.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Currency Countries ---'

insert into ores_refdata_currency_countries_tbl (
    tenant_id, currency_iso_code, version, country_alpha2_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- EUR spans the Eurozone
    (ores_utility_system_tenant_id_fn(), 'EUR', 0, 'DE',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR', 0, 'FR',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR', 0, 'IT',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR', 0, 'ES',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR', 0, 'AT',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR', 0, 'FI',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    -- Single-country currencies
    (ores_utility_system_tenant_id_fn(), 'USD', 0, 'US',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'GBP', 0, 'GB',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'JPY', 0, 'JP',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'CHF', 0, 'CH',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'CAD', 0, 'CA',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'AUD', 0, 'AU',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'SEK', 0, 'SE',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'NOK', 0, 'NO',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'DKK', 0, 'DK',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'CNY', 0, 'CN',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'HKD', 0, 'HK',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'SGD', 0, 'SG',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'ZAR', 0, 'ZA',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'INR', 0, 'IN',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'KRW', 0, 'KR',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'MXN', 0, 'MX',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'BRL', 0, 'BR',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    -- Additional single-country currencies (extends refdata.currency_calendars'
    -- country-derived coverage -- see that seed script's own header)
    (ores_utility_system_tenant_id_fn(), 'ARS', 0, 'AR',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'CLP', 0, 'CL',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'CZK', 0, 'CZ',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'HUF', 0, 'HU',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'ISK', 0, 'IS',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'PLN', 0, 'PL',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'RON', 0, 'RO',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'RUB', 0, 'RU',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'SAR', 0, 'SA',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'THB', 0, 'TH',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'TRY', 0, 'TR',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'TWD', 0, 'TW',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'UAH', 0, 'UA',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'ILS', 0, 'IL',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'IDR', 0, 'ID',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data'),
    (ores_utility_system_tenant_id_fn(), 'BWP', 0, 'BW',
     current_user, current_user, 'system.initial_load', 'Currency issuing-country reference data')
on conflict (tenant_id, currency_iso_code, country_alpha2_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_currency_countries' as entity, count(*) as count
from ores_refdata_currency_countries_tbl;
