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
 * Countries Population Script
 *
 * Seeds the ISO 3166-1 "ZZ" user-assigned sentinel country, representing
 * "supranational / not country-specific" -- used by calendar rows (e.g.
 * TARGET) with no single owning country. ZZ is one of ISO 3166-1's own
 * reserved user-assigned code elements (AA, QM-QZ, XA-XZ, ZZ), never
 * allocated to a real country, so this is real reference data rather
 * than fictional test data.
 *
 * Also seeds real ISO 3166-1 country rows for exactly the countries
 * referenced by the QuantLib calendar seed data
 * (refdata_calendars_populate.sql) -- not the full ISO country list,
 * to keep scope proportional to what calendar.country_code actually
 * needs. Extend this list if a future calendar addition references a
 * country not yet present here.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Countries (ZZ sentinel + calendar-referenced ISO countries) ---'

insert into ores_refdata_countries_tbl (
    tenant_id, alpha2_code, version, alpha3_code, numeric_code, name, official_name,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'ZZ', 0, 'ZZZ', '999',
     'Supranational / Not Country-Specific', 'Supranational / Not Country-Specific',
     current_user, current_user, 'system.initial_load',
     'ISO 3166-1 user-assigned sentinel for supranational calendars'),
    (ores_utility_system_tenant_id_fn(), 'AR', 0, 'ARG', '032', 'Argentina', 'Argentine Republic',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'AT', 0, 'AUT', '040', 'Austria', 'Republic of Austria',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'AU', 0, 'AUS', '036', 'Australia', 'Commonwealth of Australia',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'BR', 0, 'BRA', '076', 'Brazil', 'Federative Republic of Brazil',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'BW', 0, 'BWA', '072', 'Botswana', 'Republic of Botswana',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'CA', 0, 'CAN', '124', 'Canada', 'Canada',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'CH', 0, 'CHE', '756', 'Switzerland', 'Swiss Confederation',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'CL', 0, 'CHL', '152', 'Chile', 'Republic of Chile',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'CN', 0, 'CHN', '156', 'China', 'People''s Republic of China',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'CZ', 0, 'CZE', '203', 'Czechia', 'Czech Republic',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'DE', 0, 'DEU', '276', 'Germany', 'Federal Republic of Germany',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'DK', 0, 'DNK', '208', 'Denmark', 'Kingdom of Denmark',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'FI', 0, 'FIN', '246', 'Finland', 'Republic of Finland',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'FR', 0, 'FRA', '250', 'France', 'French Republic',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'GB', 0, 'GBR', '826', 'United Kingdom', 'United Kingdom of Great Britain and Northern Ireland',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'HK', 0, 'HKG', '344', 'Hong Kong', 'Hong Kong Special Administrative Region of China',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'HU', 0, 'HUN', '348', 'Hungary', 'Hungary',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'ID', 0, 'IDN', '360', 'Indonesia', 'Republic of Indonesia',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'IL', 0, 'ISR', '376', 'Israel', 'State of Israel',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'IN', 0, 'IND', '356', 'India', 'Republic of India',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'IS', 0, 'ISL', '352', 'Iceland', 'Republic of Iceland',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'IT', 0, 'ITA', '380', 'Italy', 'Italian Republic',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'JP', 0, 'JPN', '392', 'Japan', 'Japan',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'KR', 0, 'KOR', '410', 'South Korea', 'Republic of Korea',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'MX', 0, 'MEX', '484', 'Mexico', 'United Mexican States',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'NO', 0, 'NOR', '578', 'Norway', 'Kingdom of Norway',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'NZ', 0, 'NZL', '554', 'New Zealand', 'New Zealand',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'PL', 0, 'POL', '616', 'Poland', 'Republic of Poland',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'RO', 0, 'ROU', '642', 'Romania', 'Romania',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'RU', 0, 'RUS', '643', 'Russia', 'Russian Federation',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'SA', 0, 'SAU', '682', 'Saudi Arabia', 'Kingdom of Saudi Arabia',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'SE', 0, 'SWE', '752', 'Sweden', 'Kingdom of Sweden',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'SG', 0, 'SGP', '702', 'Singapore', 'Republic of Singapore',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'SK', 0, 'SVK', '703', 'Slovakia', 'Slovak Republic',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'TH', 0, 'THA', '764', 'Thailand', 'Kingdom of Thailand',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'TR', 0, 'TUR', '792', 'Turkey', 'Republic of Turkey',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'TW', 0, 'TWN', '158', 'Taiwan', 'Taiwan, Province of China',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'UA', 0, 'UKR', '804', 'Ukraine', 'Ukraine',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'US', 0, 'USA', '840', 'United States', 'United States of America',
     current_user, current_user, 'system.initial_load', 'Initial population of countries'),
    (ores_utility_system_tenant_id_fn(), 'ZA', 0, 'ZAF', '710', 'South Africa', 'Republic of South Africa',
     current_user, current_user, 'system.initial_load', 'Initial population of countries')
on conflict (tenant_id, alpha2_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    alpha3_code = excluded.alpha3_code,
    numeric_code = excluded.numeric_code,
    name = excluded.name,
    official_name = excluded.official_name,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_countries' as entity, count(*) as count
from ores_refdata_countries_tbl;
