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
 * Return Type Population Script
 *
 * Seeds the closed equity leg return type set. ORE states
 * EquityLegData.ReturnType as a bare xs:string, so the rows are the values the
 * ORE example corpus uses: every ReturnType element under external/ore/examples
 * is Total or Price.
 *
 * This script is idempotent.
 */

\echo '--- Return Type ---'

insert into ores_trading_return_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Total', ores_utility_system_tenant_id_fn(), 0, 'Total return: the leg pays the full price return',
     current_user, 'system.initial_load', 'Seed return types'),
    ('Price', ores_utility_system_tenant_id_fn(), 0, 'Price return: the leg pays the capital price return only',
     current_user, 'system.initial_load', 'Seed return types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Return Type' as entity, count(*) as count
from ores_trading_return_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
