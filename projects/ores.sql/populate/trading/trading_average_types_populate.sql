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
 * Average Type Population Script
 *
 * Seeds the trading-local averaging method set: Arithmetic and Geometric.
 * ORE states no averaging member or enumeration for commodityAveragePriceOptionData
 * or singleUnderlyingAsianOptionData, so this set is the two values the columns
 * already deploy and is local by decision; it is not read from an ORE schema.
 *
 * This script is idempotent.
 */

\echo '--- Average Type ---'

insert into ores_trading_average_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Arithmetic', ores_utility_system_tenant_id_fn(), 0, 'Arithmetic average of the observed fixings',
     current_user, 'system.initial_load', 'Seed average types'),
    ('Geometric', ores_utility_system_tenant_id_fn(), 0, 'Geometric average of the observed fixings',
     current_user, 'system.initial_load', 'Seed average types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Average Type' as entity, count(*) as count
from ores_trading_average_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
