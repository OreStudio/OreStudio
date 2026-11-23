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
 * Fixing Sources Population Script
 *
 * The fixing sources of the page "Fixing sources for option cuts and
 * non-deliverable currencies": the WM/R cut sources, the central-bank FX
 * reference rates, the settlement rate options of the non-deliverable
 * currencies and the interest rate indices. LIBOR is ceased.
 *
 * Depends on the fixing source kinds and statuses.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Fixing Sources ---'

insert into ores_refdata_fixing_sources_tbl (
    tenant_id, code, version, name, description,
    kind_code, asset_class, administrator, publication_time, publication_time_zone,
    calendar_code, rounding_decimals, status_code, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'WMR1000NYC', 0, 'WM/R New York 10:00',
     'WM/Reuters intraday spot at 10:00 New York, the source of the New York option cut.',
     'published', 'fx', 'LSEG', '10:00:00', 'America/New_York', 'UnitedStates.Settlement', 4, 'active', 1,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'WMR1500TKY', 0, 'WM/R Tokyo 15:00',
     'WM/Reuters intraday spot at 15:00 Tokyo, the source of the Tokyo option cut.',
     'published', 'fx', 'LSEG', '15:00:00', 'Asia/Tokyo', 'Japan', 4, 'active', 2,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'WMR1600LON', 0, 'WM/R London 16:00',
     'WM/Reuters closing spot at 16:00 London, the dominant FX benchmark.',
     'published', 'fx', 'LSEG', '16:00:00', 'Europe/London', 'UnitedKingdom.Settlement', 4, 'active', 3,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'WMR1200LON', 0, 'WM/R London 12:00',
     'WM/Reuters intraday spot at 12:00 London, the source of the London noon option cut.',
     'published', 'fx', 'LSEG', '12:00:00', 'Europe/London', 'UnitedKingdom.Settlement', 4, 'active', 4,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'WMR1100WAR', 0, 'WM/R Warsaw 11:00',
     'WM/Reuters intraday spot at 11:00 Warsaw, the source of the Warsaw option cut.',
     'published', 'fx', 'LSEG', '11:00:00', 'Europe/Warsaw', 'Poland', 4, 'active', 5,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'WMR1200BUD', 0, 'WM/R Budapest 12:00',
     'WM/Reuters intraday spot at 12:00 Budapest, the source of the Budapest option cut.',
     'published', 'fx', 'LSEG', '12:00:00', 'Europe/Budapest', 'Hungary', 4, 'active', 6,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'WMR1230NYC', 0, 'WM/R New York 12:30',
     'WM/Reuters half-hourly spot at 12:30 New York, the source of the New York 12:30 option cut.',
     'published', 'fx', 'LSEG', '12:30:00', 'America/New_York', 'UnitedStates.Settlement', 4, 'active', 7,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'ECB', 0, 'ECB euro reference rates',
     'The euro foreign exchange reference rates of the European Central Bank.',
     'published', 'fx', 'European Central Bank', '14:15:00', 'Europe/Berlin', 'TARGET', 4, 'active', 8,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'BOC', 0, 'Bank of Canada exchange rates',
     'The daily exchange rates of the Bank of Canada.',
     'published', 'fx', 'Bank of Canada', '16:30:00', 'America/Toronto', 'Canada.Settlement', 4, 'active', 9,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'IMF', 0, 'IMF representative exchange rates',
     'The representative exchange rates the International Monetary Fund publishes for its member currencies.',
     'published', 'fx', 'International Monetary Fund', '12:00:00', 'Europe/London', 'WeekendsOnly', 4, 'active', 10,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'TCMB', 0, 'CBRT indicative exchange rates',
     'The indicative exchange rates of the Central Bank of the Republic of Türkiye.',
     'published', 'fx', 'Central Bank of the Republic of Türkiye', '15:30:00', 'Europe/Istanbul', 'Turkey', 4, 'active', 11,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'KFTC18', 0, 'KRW KFTC18',
     'The KRW settlement rate option: the market average rate of Seoul Money Brokerage Services.',
     'published', 'fx', 'Seoul Money Brokerage Services', '15:30:00', 'Asia/Seoul', 'SouthKorea.Settlement', 2, 'active', 12,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'TAIFX1', 0, 'TWD TAIFX1',
     'The TWD settlement rate option: the Taipei Forex Inc. fixing.',
     'published', 'fx', 'Taipei Forex Inc.', '11:00:00', 'Asia/Taipei', 'Taiwan', 4, 'active', 13,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'SAEC', 0, 'CNY SAEC',
     'The CNY settlement rate option: the central parity rate of the China Foreign Exchange Trade System.',
     'published', 'fx', 'China Foreign Exchange Trade System', '09:15:00', 'Asia/Shanghai', 'China.IB', 4, 'active', 14,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'FBIL', 0, 'INR FBIL reference rate',
     'The INR settlement rate option: the FBIL reference rate, formerly RBIB.',
     'published', 'fx', 'Financial Benchmarks India', '13:30:00', 'Asia/Kolkata', 'India', 4, 'active', 15,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'PDSPESO', 0, 'PHP PDSPESO',
     'The PHP settlement rate option: the weighted average rate of the Philippine Dealing System.',
     'published', 'fx', 'Philippine Dealing System', '11:30:00', 'Asia/Manila', 'WeekendsOnly', 3, 'active', 16,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'JISDOR', 0, 'IDR JISDOR',
     'The IDR settlement rate option: the Bank Indonesia JISDOR rate, which replaced ABSIRFIX01.',
     'published', 'fx', 'Bank Indonesia', '10:00:00', 'Asia/Jakarta', 'Indonesia.IDX', 0, 'active', 17,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'PTAX', 0, 'BRL PTAX',
     'The BRL settlement rate option (BRL09): the PTAX rate of the Banco Central do Brasil.',
     'published', 'fx', 'Banco Central do Brasil', '13:00:00', 'America/Sao_Paulo', 'Brazil', 4, 'active', 18,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'SOFR', 0, 'Secured Overnight Financing Rate',
     'The USD overnight risk-free rate.',
     'published', 'ir', 'Federal Reserve Bank of New York', '08:00:00', 'America/New_York', 'UnitedStates.SOFR', 2, 'active', 19,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'SONIA', 0, 'Sterling Overnight Index Average',
     'The GBP overnight risk-free rate.',
     'published', 'ir', 'Bank of England', '09:00:00', 'Europe/London', 'UnitedKingdom.Settlement', 4, 'active', 20,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'ESTR', 0, 'Euro short-term rate',
     'The EUR overnight risk-free rate.',
     'published', 'ir', 'European Central Bank', '08:00:00', 'Europe/Berlin', 'TARGET', 3, 'active', 21,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'EURIBOR', 0, 'Euro Interbank Offered Rate',
     'The EUR term rates.',
     'published', 'ir', 'European Money Markets Institute', '11:00:00', 'Europe/Brussels', 'TARGET', 3, 'active', 22,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'HIBOR', 0, 'Hong Kong Interbank Offered Rate',
     'The HKD term rates.',
     'published', 'ir', 'Treasury Markets Association', '11:15:00', 'Asia/Hong_Kong', 'HongKong', 5, 'active', 23,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources'),
    (ores_utility_system_tenant_id_fn(), 'LIBOR', 0, 'London Interbank Offered Rate',
     'The USD, GBP, CHF and JPY term rates, kept for teaching and for historical trades.',
     'published', 'ir', 'ICE Benchmark Administration', '11:55:00', 'Europe/London', 'UnitedKingdom.Settlement', 5, 'ceased', 24,
     current_user, current_user, 'system.initial_load', 'Initial population of fixing sources')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
