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
 * Currency Market Tiers Population Script
 *
 * Populates valid currency market tier categories.
 * Classifies currencies by their liquidity and market accessibility.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE so that
 * description text is kept up-to-date in a single atomic statement.
 */

\echo '--- Currency Market Tiers ---'

insert into ores_refdata_currency_market_tiers_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'g10', 0, 'G10 / Majors',
     'Major currencies with deep liquidity and tight spreads',
     1, current_user, current_user, 'system.initial_load', 'Initial population of currency market tiers'),
    (ores_iam_system_tenant_id_fn(), 'emerging', 0, 'Emerging',
     'Currencies from developing economies with moderate liquidity',
     2, current_user, current_user, 'system.initial_load', 'Initial population of currency market tiers'),
    (ores_iam_system_tenant_id_fn(), 'exotic', 0, 'Exotic',
     'Thinly traded currencies with wide spreads',
     3, current_user, current_user, 'system.initial_load', 'Initial population of currency market tiers'),
    (ores_iam_system_tenant_id_fn(), 'frontier', 0, 'Frontier',
     'Currencies from frontier markets with limited convertibility',
     4, current_user, current_user, 'system.initial_load', 'Initial population of currency market tiers'),
    (ores_iam_system_tenant_id_fn(), 'historical', 0, 'Historical',
     'Currencies no longer in active use',
     5, current_user, current_user, 'system.initial_load', 'Initial population of currency market tiers')
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
select 'refdata_currency_market_tiers' as entity, count(*) as count
from ores_refdata_currency_market_tiers_tbl;
