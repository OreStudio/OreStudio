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
 * Sensitivity Methods Population Script
 *
 * The five methods a sensitivity template key spells: the token ORE writes in
 * the key, the words a person reads, and whether a template that states the
 * method also states an asset class.
 *
 * Four of the five price a class of product, so their templates state the class.
 * Trade level is the exception: the trade states its own sensitivities, so
 * nothing is shifted and no class is named.
 *
 * This script is idempotent.
 */

\echo '--- Sensitivity Methods ---'

insert into ores_analytics_sensitivity_methods_tbl (
    code, tenant_id, version, name, description, needs_asset_class, display_order,
    modified_by, change_reason_code, change_commentary
) values
    ('Analytical', ores_utility_system_tenant_id_fn(), 0, 'Analytical',
     'A closed formula prices the product, so no step of the pricing is evaluated numerically.',
     true, 1, 'ores_analytics_service', 'system.initial_load', 'Seed the sensitivity methods'),
    ('Semianalytical', ores_utility_system_tenant_id_fn(), 0, 'Semianalytical',
     'A closed formula prices the product, and one step of it is evaluated numerically, as CMS replication is.',
     true, 2, 'ores_analytics_service', 'system.initial_load', 'Seed the sensitivity methods'),
    ('FD', ores_utility_system_tenant_id_fn(), 0, 'Finite difference',
     'The product is priced on a finite difference grid, which an early-exercise right such as a Bermudan needs.',
     true, 3, 'ores_analytics_service', 'system.initial_load', 'Seed the sensitivity methods'),
    ('MC', ores_utility_system_tenant_id_fn(), 0, 'Monte Carlo',
     'The product is priced by simulating paths of its risk factors.',
     true, 4, 'ores_analytics_service', 'system.initial_load', 'Seed the sensitivity methods'),
    ('Trade', ores_utility_system_tenant_id_fn(), 0, 'Trade level',
     'The trade states its own sensitivities, so the market is not shifted to obtain them and no asset class is named.',
     false, 5, 'ores_analytics_service', 'system.initial_load', 'Seed the sensitivity methods')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Sensitivity Methods' as entity, count(*) as count
from ores_analytics_sensitivity_methods_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
