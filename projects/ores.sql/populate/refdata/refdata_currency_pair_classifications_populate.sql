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
 * Currency Pair Classifications Population Script
 *
 * Populates the liquidity classes a currency pair is placed in: the codes the
 * seeded pairs already carry, so the pair screen can name them and a new pair
 * can pick one.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE so that
 * description text is kept up-to-date in a single atomic statement.
 */

\echo '--- Currency Pair Classifications ---'

insert into ores_refdata_currency_pair_classifications_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'major', 0, 'Major',
     'The most traded pairs, each against the US dollar',
     1, current_user, current_user, 'system.initial_load', 'Initial population of currency pair classifications'),
    (ores_utility_system_tenant_id_fn(), 'minor', 0, 'Minor',
     'Crosses between major currencies that do not involve the US dollar',
     2, current_user, current_user, 'system.initial_load', 'Initial population of currency pair classifications'),
    (ores_utility_system_tenant_id_fn(), 'exotic', 0, 'Exotic',
     'A major currency against an emerging or thinly traded one',
     3, current_user, current_user, 'system.initial_load', 'Initial population of currency pair classifications'),
    (ores_utility_system_tenant_id_fn(), 'commodity', 0, 'Commodity',
     'A precious metal or another commodity quoted as a currency pair',
     4, current_user, current_user, 'system.initial_load', 'Initial population of currency pair classifications')
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
select 'refdata_currency_pair_classifications' as entity, count(*) as count
from ores_refdata_currency_pair_classifications_tbl;
