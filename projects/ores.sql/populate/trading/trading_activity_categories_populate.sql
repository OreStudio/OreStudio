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
 * Activity Category Population Script
 *
 * Seeds the closed activity category set. The rows are the categories
 * projects/ores.sql/populate/trading/trading_activity_types_populate.sql already uses.
 *
 * This script is idempotent.
 */

\echo '--- Activity Category ---'

insert into ores_trading_activity_categories_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('new_activity', ores_utility_system_tenant_id_fn(), 0, 'An activity that creates a new trade record',
     current_user, 'system.initial_load', 'Seed activity_category'),
    ('lifecycle_event', ores_utility_system_tenant_id_fn(), 0, 'An activity that moves a live trade',
     current_user, 'system.initial_load', 'Seed activity_category'),
    ('misbooking', ores_utility_system_tenant_id_fn(), 0, 'An activity that corrects a booking error',
     current_user, 'system.initial_load', 'Seed activity_category'),
    ('valuation_change', ores_utility_system_tenant_id_fn(), 0, 'An activity that changes a valuation input',
     current_user, 'system.initial_load', 'Seed activity_category'),
    ('cancellation', ores_utility_system_tenant_id_fn(), 0, 'An activity that cancels a trade',
     current_user, 'system.initial_load', 'Seed activity_category')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Activity Category' as entity, count(*) as count
from ores_trading_activity_categories_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
