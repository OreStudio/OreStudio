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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * Structure Reference Data Population Script
 *
 * Seeds the composition ladder's rungs, the templates that shape a rung,
 * and the roles each template allows with their leg counts. The platform
 * defines this vocabulary, so it ships with the system and an installation
 * does not choose its own values. This script is idempotent.
 *
 * A maximum of zero means the role states no upper bound, which is what a
 * dynamically typed product needs when its roles are read from data.
 */

\echo '--- Structure Kinds ---'

insert into ores_trading_structure_kinds_tbl (
    code, tenant_id, version, description, confirms_as_whole,
    modified_by, change_reason_code, change_commentary
) values
    ('Strategy', ores_utility_system_tenant_id_fn(), 0,
     'One product family under one pricing model, with fixed leg roles; priced as a whole',
     true, current_user, 'system.initial_load', 'Seed structure kinds'),

    ('Typed', ores_utility_system_tenant_id_fn(), 0,
     'Legs from different families with the roles fixed in the product type; priced as a whole',
     true, current_user, 'system.initial_load', 'Seed structure kinds'),

    ('Dynamic', ores_utility_system_tenant_id_fn(), 0,
     'Roles read from data at run time, as a generic scripted product does',
     true, current_user, 'system.initial_load', 'Seed structure kinds'),

    ('Package', ores_utility_system_tenant_id_fn(), 0,
     'A container that binds its legs to nothing economic, so each leg confirms on its own',
     false, current_user, 'system.initial_load', 'Seed structure kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Structure Kinds' as entity, count(*) as count
from ores_trading_structure_kinds_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

\echo '--- Structure Templates ---'

insert into ores_trading_structure_templates_tbl (
    code, tenant_id, version, description, kind,
    modified_by, change_reason_code, change_commentary
) values
    ('Straddle', ores_utility_system_tenant_id_fn(), 0,
     'A call and a put on the same underlying at the same expiry and strike',
     'Strategy', current_user, 'system.initial_load', 'Seed structure templates'),

    ('RiskReversal', ores_utility_system_tenant_id_fn(), 0,
     'A long call and a short put, or the reverse, at the same expiry',
     'Strategy', current_user, 'system.initial_load', 'Seed structure templates'),

    ('Butterfly', ores_utility_system_tenant_id_fn(), 0,
     'A body and two wings, struck so that the position is neutral at the outer strikes',
     'Strategy', current_user, 'system.initial_load', 'Seed structure templates'),

    ('CalendarSpread', ores_utility_system_tenant_id_fn(), 0,
     'The same option bought at one expiry and sold at another',
     'Strategy', current_user, 'system.initial_load', 'Seed structure templates'),

    ('CallableSwap', ores_utility_system_tenant_id_fn(), 0,
     'A swap with an embedded right to terminate, priced as one instrument',
     'Typed', current_user, 'system.initial_load', 'Seed structure templates')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Structure Templates' as entity, count(*) as count
from ores_trading_structure_templates_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

\echo '--- Structure Template Roles ---'

insert into ores_trading_structure_template_roles_tbl (
    template_code, role, tenant_id, version, min_legs, max_legs, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Straddle', 'leg', ores_utility_system_tenant_id_fn(), 0, 2, 2,
     'The two legs are one call and one put, and neither stands alone',
     current_user, 'system.initial_load', 'Seed structure template roles'),

    ('RiskReversal', 'long', ores_utility_system_tenant_id_fn(), 0, 1, 1,
     'The leg the position is long, which may be the call or the put',
     current_user, 'system.initial_load', 'Seed structure template roles'),

    ('RiskReversal', 'short', ores_utility_system_tenant_id_fn(), 0, 1, 1,
     'The leg the position is short, which may be the put or the call',
     current_user, 'system.initial_load', 'Seed structure template roles'),

    ('Butterfly', 'body', ores_utility_system_tenant_id_fn(), 0, 1, 1,
     'The central strike, held at double the weight of a wing',
     current_user, 'system.initial_load', 'Seed structure template roles'),

    ('Butterfly', 'wing', ores_utility_system_tenant_id_fn(), 0, 2, 2,
     'The two outer strikes, one on each side of the body',
     current_user, 'system.initial_load', 'Seed structure template roles'),

    ('CalendarSpread', 'near', ores_utility_system_tenant_id_fn(), 0, 1, 1,
     'The leg that expires first',
     current_user, 'system.initial_load', 'Seed structure template roles'),

    ('CalendarSpread', 'far', ores_utility_system_tenant_id_fn(), 0, 1, 1,
     'The leg that expires last',
     current_user, 'system.initial_load', 'Seed structure template roles'),

    ('CallableSwap', 'swap', ores_utility_system_tenant_id_fn(), 0, 1, 1,
     'The underlying swap the call right attaches to',
     current_user, 'system.initial_load', 'Seed structure template roles'),

    ('CallableSwap', 'call', ores_utility_system_tenant_id_fn(), 0, 1, 1,
     'The embedded right to terminate, which is not a leg that carries cash',
     current_user, 'system.initial_load', 'Seed structure template roles')
on conflict (tenant_id, template_code, role)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Structure Template Roles' as entity, count(*) as count
from ores_trading_structure_template_roles_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
