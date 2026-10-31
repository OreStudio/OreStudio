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
 * Business Day Convention Types Population Script
 *
 * Seeds the database with ORE business day adjustment convention codes.
 * Values sourced from ORE ore_types.xsd BusinessDayConvention enumeration.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE so that
 * name/description text is kept up-to-date in a single atomic statement.
 */

\echo '--- Business Day Convention Types ---'

insert into ores_refdata_business_day_convention_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'Following', 0, 'Following',
     'Adjust to the next good business day',
     1, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'ModifiedFollowing', 0, 'Modified Following',
     'Next good business day unless it crosses a month end, in which case the previous good day',
     2, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'Preceding', 0, 'Preceding',
     'Adjust to the previous good business day',
     3, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'ModifiedPreceding', 0, 'Modified Preceding',
     'Previous good business day unless it crosses a month start, in which case the next good day',
     4, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'Unadjusted', 0, 'Unadjusted',
     'No business day adjustment applied',
     5, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'HalfMonthModifiedFollowing', 0, 'Half-Month Modified Following',
     'Modified Following, additionally rolled back within the same half-month period',
     6, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'Nearest', 0, 'Nearest',
     'Adjust to the nearest good business day, forward or backward',
     7, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types')
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
select 'refdata_business_day_convention_types' as entity, count(*) as count
from ores_refdata_business_day_convention_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
