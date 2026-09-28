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
 * Payoff Type Population Script
 *
 * Seeds the closed payoff set ORE states on optionData.PayoffType. ORE declares
 * that element as a bare xs:string, so the rows are the values the ORE example
 * corpus uses: Accumulator, Asian, AverageStrike, Decumulator, TargetExact,
 * TargetFull and Vanilla.
 *
 * This script is idempotent.
 */

\echo '--- Payoff Type ---'

insert into ores_trading_payoff_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Accumulator',   ores_utility_system_tenant_id_fn(), 0, 'Accumulator payoff',
     current_user, 'system.initial_load', 'Seed payoff types'),
    ('Asian',         ores_utility_system_tenant_id_fn(), 0, 'Asian averaging payoff',
     current_user, 'system.initial_load', 'Seed payoff types'),
    ('AverageStrike', ores_utility_system_tenant_id_fn(), 0, 'Average strike payoff',
     current_user, 'system.initial_load', 'Seed payoff types'),
    ('Decumulator',   ores_utility_system_tenant_id_fn(), 0, 'Decumulator payoff',
     current_user, 'system.initial_load', 'Seed payoff types'),
    ('TargetExact',   ores_utility_system_tenant_id_fn(), 0, 'Target exact: the target amount is fixed',
     current_user, 'system.initial_load', 'Seed payoff types'),
    ('TargetFull',    ores_utility_system_tenant_id_fn(), 0, 'Target full: the target amount is a maximum',
     current_user, 'system.initial_load', 'Seed payoff types'),
    ('Vanilla',       ores_utility_system_tenant_id_fn(), 0, 'Vanilla payoff',
     current_user, 'system.initial_load', 'Seed payoff types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Payoff Type' as entity, count(*) as count
from ores_trading_payoff_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
