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
 * Cut Codes Population Script
 *
 * The option expiry cuts ORE Studio supports, each with the WM/R source that
 * fixes the market at its instant. Depends on the fixing sources.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Cut Codes ---'

insert into ores_refdata_cut_codes_tbl (
    tenant_id, code, version, name, description,
    cut_time, cut_time_zone, fixing_source_code, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'NYC1000', 0, 'New York 10:00',
     'The New York option cut.',
     '10:00:00', 'America/New_York', 'WMR1000NYC', 1,
     current_user, current_user, 'system.initial_load', 'Initial population of cut codes'),
    (ores_utility_system_tenant_id_fn(), 'TKY1500', 0, 'Tokyo 15:00',
     'The Tokyo option cut.',
     '15:00:00', 'Asia/Tokyo', 'WMR1500TKY', 2,
     current_user, current_user, 'system.initial_load', 'Initial population of cut codes'),
    (ores_utility_system_tenant_id_fn(), 'LON1600', 0, 'London 16:00',
     'The London close, the time of the WM/R closing spot.',
     '16:00:00', 'Europe/London', 'WMR1600LON', 3,
     current_user, current_user, 'system.initial_load', 'Initial population of cut codes'),
    (ores_utility_system_tenant_id_fn(), 'LON1200', 0, 'London 12:00',
     'The London noon option cut.',
     '12:00:00', 'Europe/London', 'WMR1200LON', 4,
     current_user, current_user, 'system.initial_load', 'Initial population of cut codes'),
    (ores_utility_system_tenant_id_fn(), 'WAR1100', 0, 'Warsaw 11:00',
     'The Warsaw option cut.',
     '11:00:00', 'Europe/Warsaw', 'WMR1100WAR', 5,
     current_user, current_user, 'system.initial_load', 'Initial population of cut codes'),
    (ores_utility_system_tenant_id_fn(), 'BUD1200', 0, 'Budapest 12:00',
     'The Budapest option cut.',
     '12:00:00', 'Europe/Budapest', 'WMR1200BUD', 6,
     current_user, current_user, 'system.initial_load', 'Initial population of cut codes'),
    (ores_utility_system_tenant_id_fn(), 'NYC1230', 0, 'New York 12:30',
     'The New York 12:30 option cut.',
     '12:30:00', 'America/New_York', 'WMR1230NYC', 7,
     current_user, current_user, 'system.initial_load', 'Initial population of cut codes')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
