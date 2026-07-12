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
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE so that
 * name/description text is kept up-to-date in a single atomic statement.
 */

\echo '--- Day Count Fraction Types ---'

insert into ores_refdata_day_count_fraction_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'A360', 0, 'Actual/360',
     'Actual/360', 1, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), 'A365F', 0, 'Actual/365 (Fixed)',
     'Actual/365 (Fixed)', 2, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), 'A365', 0, 'Actual/365',
     'Actual/365', 3, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), 'ActAct(ISDA)', 0, 'Actual/Actual (ISDA)',
     'Actual/Actual (ISDA)', 4, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), 'ActAct(ISMA)', 0, 'Actual/Actual (ISMA)',
     'Actual/Actual (ISMA)', 5, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), 'ActAct(AFB)', 0, 'Actual/Actual (AFB)',
     'Actual/Actual (AFB)', 6, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), '30/360', 0, '30/360 (Bond Basis)',
     '30/360 (Bond Basis)', 7, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), '30E/360', 0, '30E/360 (Eurobond Basis)',
     '30E/360 (Eurobond Basis)', 8, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), '30E/360(ISDA)', 0, '30E/360 (ISDA)',
     '30E/360 (ISDA)', 9, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), 'Business252', 0, 'Business/252',
     'Business/252 (Brazilian convention)', 10, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), '1/1', 0, '1/1',
     '1/1 (unit day count)', 11, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types'),
    (ores_utility_system_tenant_id_fn(), 'Simple', 0, 'Simple',
     'Simple day count', 12, current_user, current_user, 'system.initial_load', 'Initial population of day count fraction types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    name = excluded.name,
    description = excluded.description,
    display_order = excluded.display_order,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_day_count_fraction_types' as entity, count(*) as count
from ores_refdata_day_count_fraction_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
