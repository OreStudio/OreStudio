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
 * Currency Pair Convention Calendars Population Script
 *
 * Seeds real currency-pair-convention-to-calendar mappings for the
 * major pairs already present in seed data, mirroring what
 * ORE's own AdvanceCalendar convention names -- the union of both
 * legs' calendars (e.g. EUR/USD -> TARGET, UnitedStates.Settlement).
 * Real reference data, not fictional test data. Replaces the
 * comma-joined values previously seeded on
 * currency_pair_convention.advance_calendar.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Currency Pair Convention Calendars ---'

insert into ores_refdata_currency_pair_convention_calendars_tbl (
    tenant_id, pair_code, version, calendar_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'EUR/USD', 0, 'TARGET',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR/USD', 0, 'UnitedStates.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'GBP/USD', 0, 'UnitedKingdom.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'GBP/USD', 0, 'UnitedStates.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'USD/JPY', 0, 'UnitedStates.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'USD/JPY', 0, 'Japan',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'USD/CHF', 0, 'UnitedStates.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'USD/CHF', 0, 'Switzerland',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'USD/CAD', 0, 'UnitedStates.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'USD/CAD', 0, 'Canada.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'AUD/USD', 0, 'Australia',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'AUD/USD', 0, 'UnitedStates.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR/GBP', 0, 'TARGET',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR/GBP', 0, 'UnitedKingdom.Settlement',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR/JPY', 0, 'TARGET',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data'),
    (ores_utility_system_tenant_id_fn(), 'EUR/JPY', 0, 'Japan',
     current_user, current_user, 'system.initial_load', 'Currency pair convention advance-calendar reference data')
on conflict (tenant_id, pair_code, calendar_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_currency_pair_convention_calendars' as entity, count(*) as count
from ores_refdata_currency_pair_convention_calendars_tbl;
