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
 * Origin Types Population Script
 *
 * The closed set of systems a trade can come from. A code with no C++ enum
 * value cannot be read back, so the set is seeded once and never changes.
 *
 * This script is idempotent.
 */

insert into ores_trading_origin_types_tbl (
    code, version, description, modified_by, change_reason_code, change_commentary
) values
    ('ux',         0, 'A person entered the trade at a screen',   current_user, 'system.initial_load', 'Seed origin_type'),
    ('api',        0, 'An external caller wrote it through the API', current_user, 'system.initial_load', 'Seed origin_type'),
    ('ore_import', 0, 'The ORE XML import wrote it',              current_user, 'system.initial_load', 'Seed origin_type'),
    ('synthetic',  0, 'The trade population generator wrote it',  current_user, 'system.initial_load', 'Seed origin_type')
on conflict (code, version)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

\echo ''
\echo '--- Trading Origin Types Summary ---'
select 'Total Trading Origin Types' as metric, count(*) as count
from ores_trading_origin_types_tbl;
