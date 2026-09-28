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
 * Long Short Types Population Script
 *
 * Seeds the closed position-direction set. The rows are the ORE longShort
 * simple type under external/ore/xsd/ore_types.xsd: exactly Long and Short,
 * in the XSD's own spelling, so a stored value round-trips through the ORE
 * XML unchanged.
 *
 * This script is idempotent.
 */

\echo '--- Long Short Types ---'

insert into ores_trading_long_short_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Long',  ores_utility_system_tenant_id_fn(), 0, 'Long position',
     current_user, 'system.initial_load', 'Seed long short types'),
    ('Short', ores_utility_system_tenant_id_fn(), 0, 'Short position',
     current_user, 'system.initial_load', 'Seed long short types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Long Short Types' as entity, count(*) as count
from ores_trading_long_short_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
