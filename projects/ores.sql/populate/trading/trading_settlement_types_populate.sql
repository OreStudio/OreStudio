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
 * Settlement Type Population Script
 *
 * Seeds the closed option settlement set. The rows are the ORE
 * settlementType simple type under external/ore/xsd/ore_types.xsd.
 *
 * This script is idempotent.
 */

\echo '--- Settlement Type ---'

insert into ores_trading_settlement_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Physical', ores_utility_system_tenant_id_fn(), 0, 'Settlement delivers the underlying',
     current_user, 'system.initial_load', 'Seed settlement_type'),
    ('Cash', ores_utility_system_tenant_id_fn(), 0, 'Settlement pays the cash value',
     current_user, 'system.initial_load', 'Seed settlement_type')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Settlement Type' as entity, count(*) as count
from ores_trading_settlement_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
