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
 * Product Family Codes Population Script
 *
 * Seeds the contract-shape taxonomy a product type carries. The values are
 * ours, not a published scheme's: the analysis behind them is in the Product
 * Family knowledge note. A product type names one, or none where the corpus
 * states the shape is unsettled.
 *
 * This script is idempotent.
 */

\echo '--- Product Family Codes ---'

insert into ores_refdata_product_family_codes_tbl (
    code, tenant_id, version, name, description, display_order,
    modified_by, change_reason_code, change_commentary
) values
    ('option', ores_utility_system_tenant_id_fn(), 0, 'Option',
     'A contract that gives one party a right it may exercise, and the other an obligation.',
     1, current_user, 'system.initial_load', 'Seed product families'),
    ('swap', ores_utility_system_tenant_id_fn(), 0, 'Swap',
     'An exchange of two streams of payments between two parties.',
     2, current_user, 'system.initial_load', 'Seed product families'),
    ('forward', ores_utility_system_tenant_id_fn(), 0, 'Forward',
     'A binding agreement to trade at a future date on terms set now.',
     3, current_user, 'system.initial_load', 'Seed product families'),
    ('position', ores_utility_system_tenant_id_fn(), 0, 'Position',
     'A holding in an instrument rather than an agreement struck.',
     4, current_user, 'system.initial_load', 'Seed product families'),
    ('swaption', ores_utility_system_tenant_id_fn(), 0, 'Swaption',
     'An option whose underlying is a swap.',
     5, current_user, 'system.initial_load', 'Seed product families'),
    ('future', ores_utility_system_tenant_id_fn(), 0, 'Future',
     'An exchange-traded agreement to trade at a future date, cleared and margined.',
     6, current_user, 'system.initial_load', 'Seed product families'),
    ('repo', ores_utility_system_tenant_id_fn(), 0, 'Repo',
     'A sale and repurchase of securities, which is a secured loan in substance.',
     7, current_user, 'system.initial_load', 'Seed product families'),
    ('security', ores_utility_system_tenant_id_fn(), 0, 'Security',
     'An instrument held, which pays a defined schedule of coupons and principal.',
     8, current_user, 'system.initial_load', 'Seed product families')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Product Family Codes' as entity, count(*) as count
from ores_refdata_product_family_codes_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
