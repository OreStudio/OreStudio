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
 * Calendars Population Script
 *
 * Seeds the full QuantLib calendar set (transcribed from a local
 * QuantLib checkout's calendar headers under ql/time/calendars),
 * mirroring generate_quantlib_calendars() in calendar_generator.cpp -- real
 * reference data, not fictional test data. Sub-market variants (e.g.
 * UnitedStates.NYSE, UnitedStates.GovernmentBond) are separate rows
 * sharing the dotted QuantLib token verbatim as code, matching ORE's
 * XML <Calendar> vocabulary exactly.
 *
 * Depends on refdata_calendar_types_populate.sql (calendar_type FK)
 * and refdata_countries_populate.sql (country_code FK) having run
 * first.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Calendars (QuantLib calendar set) ---'

insert into ores_refdata_calendars_tbl (
    tenant_id, code, version, name, calendar_type, country_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- Supranational
    (ores_utility_system_tenant_id_fn(), 'TARGET', 0, 'TARGET (Euro area)', 'public_holiday', 'ZZ',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'WeekendsOnly', 0, 'Weekends Only', 'public_holiday', 'ZZ',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    -- Single-market national calendars
    (ores_utility_system_tenant_id_fn(), 'Argentina', 0, 'Argentina', 'public_holiday', 'AR',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Australia', 0, 'Australia', 'public_holiday', 'AU',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Austria', 0, 'Austria', 'public_holiday', 'AT',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Botswana', 0, 'Botswana', 'public_holiday', 'BW',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Brazil', 0, 'Brazil', 'public_holiday', 'BR',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Chile', 0, 'Chile', 'public_holiday', 'CL',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'CzechRepublic', 0, 'Czech Republic', 'public_holiday', 'CZ',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Denmark', 0, 'Denmark', 'public_holiday', 'DK',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Finland', 0, 'Finland', 'public_holiday', 'FI',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'France', 0, 'France', 'public_holiday', 'FR',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'HongKong', 0, 'Hong Kong', 'public_holiday', 'HK',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Hungary', 0, 'Hungary', 'public_holiday', 'HU',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Iceland', 0, 'Iceland', 'public_holiday', 'IS',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'India', 0, 'India (National Stock Exchange)', 'financial_centre', 'IN',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Italy', 0, 'Italy', 'public_holiday', 'IT',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Japan', 0, 'Japan', 'public_holiday', 'JP',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Mexico', 0, 'Mexico', 'public_holiday', 'MX',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'NewZealand', 0, 'New Zealand', 'public_holiday', 'NZ',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Norway', 0, 'Norway', 'public_holiday', 'NO',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Poland', 0, 'Poland', 'public_holiday', 'PL',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Romania', 0, 'Romania', 'public_holiday', 'RO',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Russia', 0, 'Russia', 'public_holiday', 'RU',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'SaudiArabia', 0, 'Saudi Arabia', 'public_holiday', 'SA',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Singapore', 0, 'Singapore', 'public_holiday', 'SG',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Slovakia', 0, 'Slovakia', 'public_holiday', 'SK',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'SouthAfrica', 0, 'South Africa', 'public_holiday', 'ZA',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Sweden', 0, 'Sweden', 'public_holiday', 'SE',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Switzerland', 0, 'Switzerland', 'public_holiday', 'CH',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Taiwan', 0, 'Taiwan', 'public_holiday', 'TW',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Thailand', 0, 'Thailand', 'public_holiday', 'TH',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Turkey', 0, 'Turkey', 'public_holiday', 'TR',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Ukraine', 0, 'Ukraine', 'public_holiday', 'UA',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    -- United States (Market enum)
    (ores_utility_system_tenant_id_fn(), 'UnitedStates.Settlement', 0, 'United States (Settlement)', 'public_holiday', 'US',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'UnitedStates.NYSE', 0, 'United States (NYSE)', 'financial_centre', 'US',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'UnitedStates.GovernmentBond', 0, 'United States (Government Bond)', 'financial_centre', 'US',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'UnitedStates.NERC', 0, 'United States (NERC)', 'financial_centre', 'US',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'UnitedStates.LiborImpact', 0, 'United States (Libor Impact)', 'financial_centre', 'US',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'UnitedStates.FederalReserve', 0, 'United States (Federal Reserve)', 'financial_centre', 'US',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'UnitedStates.SOFR', 0, 'United States (SOFR)', 'financial_centre', 'US',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    -- United Kingdom (Market enum)
    (ores_utility_system_tenant_id_fn(), 'UnitedKingdom.Settlement', 0, 'United Kingdom (Settlement)', 'public_holiday', 'GB',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'UnitedKingdom.Exchange', 0, 'United Kingdom (Exchange)', 'financial_centre', 'GB',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'UnitedKingdom.Metals', 0, 'United Kingdom (Metals)', 'financial_centre', 'GB',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    -- China (Market enum)
    (ores_utility_system_tenant_id_fn(), 'China.SSE', 0, 'China (Shanghai Stock Exchange)', 'financial_centre', 'CN',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'China.IB', 0, 'China (Interbank)', 'financial_centre', 'CN',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    -- Germany (Market enum)
    (ores_utility_system_tenant_id_fn(), 'Germany.Settlement', 0, 'Germany (Settlement)', 'public_holiday', 'DE',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Germany.FrankfurtStockExchange', 0, 'Germany (Frankfurt Stock Exchange)', 'financial_centre', 'DE',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Germany.Xetra', 0, 'Germany (Xetra)', 'financial_centre', 'DE',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Germany.Eurex', 0, 'Germany (Eurex)', 'financial_centre', 'DE',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Germany.Euwax', 0, 'Germany (Euwax)', 'financial_centre', 'DE',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    -- Canada (Market enum)
    (ores_utility_system_tenant_id_fn(), 'Canada.Settlement', 0, 'Canada (Settlement)', 'public_holiday', 'CA',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Canada.TSX', 0, 'Canada (Toronto Stock Exchange)', 'financial_centre', 'CA',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    -- South Korea (Market enum)
    (ores_utility_system_tenant_id_fn(), 'SouthKorea.Settlement', 0, 'South Korea (Settlement)', 'public_holiday', 'KR',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'SouthKorea.KRX', 0, 'South Korea (Korea Exchange)', 'financial_centre', 'KR',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    -- Indonesia (Market enum)
    (ores_utility_system_tenant_id_fn(), 'Indonesia.BEJ', 0, 'Indonesia (Jakarta SE, legacy BEJ)', 'financial_centre', 'ID',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Indonesia.JSX', 0, 'Indonesia (Jakarta SE, legacy JSX)', 'financial_centre', 'ID',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Indonesia.IDX', 0, 'Indonesia (Indonesia Stock Exchange)', 'financial_centre', 'ID',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    -- Israel (Market enum)
    (ores_utility_system_tenant_id_fn(), 'Israel.Settlement', 0, 'Israel (Settlement)', 'public_holiday', 'IL',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'Israel.TASE', 0, 'Israel (Tel-Aviv Stock Exchange)', 'financial_centre', 'IL',
     current_user, current_user, 'system.initial_load', 'QuantLib calendar reference data')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    name = excluded.name,
    calendar_type = excluded.calendar_type,
    country_code = excluded.country_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_calendars' as entity, count(*) as count
from ores_refdata_calendars_tbl;
