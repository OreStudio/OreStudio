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
 * Day Counters Population Script
 *
 * Populates the day counter spellings ORE accepts. The set is the dayCounter
 * enumeration in external/ore/xsd/ore_types.xsd, one row per spelling. The
 * description is left empty: a spelling names its day counter, and which
 * canonical day count fraction it means is not recorded yet.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Day Counters ---'

insert into ores_refdata_day_counters_tbl (
    tenant_id, code, description, version,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'A360', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/360', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/360', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Act/360', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'A360 (Incl Last)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/360 (Incl Last)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/360 (Incl Last)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'A365', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'A365F', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/365 (Fixed)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/365 (fixed)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/365.FIXED', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/365', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/365L', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Act/365', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Act/365L', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Act/365 (Canadian Bond)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'T360', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30/360', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30/360 US', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30/360 (US)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30/360 NASD', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30U/360', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30US/360', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30/360 (Bond Basis)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/nACT', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30E/360 (Eurobond Basis)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30/360 AIBD (Euro)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30E/360.ICMA', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30E/360 ICMA', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30E/360', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30E/360E', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30E/360.ISDA', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30E/360 ISDA', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30/360 German', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30/360 (German)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30/360 Italian', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '30/360 (Italian)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ActActISDA', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/ACT.ISDA', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/Actual (ISDA)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ActualActual (ISDA)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/ACT', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Act/Act', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT29', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ActActISMA', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/Actual (ISMA)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ActualActual (ISMA)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/ACT.ISMA', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ActActICMA', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/Actual (ICMA)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ActualActual (ICMA)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/ACT.ICMA', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ActActAFB', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/ACT.AFB', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/Actual (AFB)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), '1/1', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'BUS/252', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Business/252', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/365 (No Leap)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Act/365 (NL)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'NL/365', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/365 (JGB)', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Simple', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Year', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'A364', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Actual/364', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Act/364', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'ACT/364', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters'),
    (ores_utility_system_tenant_id_fn(), 'Month', null, 0,
     current_user, current_user, 'system.initial_load', 'Initial population of day counters')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_day_counters' as entity, count(*) as count
from ores_refdata_day_counters_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
