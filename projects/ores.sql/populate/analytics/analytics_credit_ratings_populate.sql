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
 * Credit Ratings Population Script
 *
 * Seeds the credit rating scale an ORE transition matrix is laid out on. The
 * labels are taken from the comment ORE writes inside every matrix Data
 * element; all fourteen credit simulation documents in the vendored corpus
 * carry these same eight, in this order. display_order is the position a
 * matrix row and column index by. This script is idempotent.
 */

\echo '--- Credit Ratings ---'

insert into ores_analytics_credit_ratings_tbl (
    code, tenant_id, version, name, display_order,
    modified_by, change_reason_code, change_commentary
) values
    ('Aaa',     ores_utility_system_tenant_id_fn(), 0, 'Aaa',     0, 'admin', 'system.initial_load', 'Seed credit ratings'),
    ('Aa',      ores_utility_system_tenant_id_fn(), 0, 'Aa',      1, 'admin', 'system.initial_load', 'Seed credit ratings'),
    ('A',       ores_utility_system_tenant_id_fn(), 0, 'A',       2, 'admin', 'system.initial_load', 'Seed credit ratings'),
    ('Baa',     ores_utility_system_tenant_id_fn(), 0, 'Baa',     3, 'admin', 'system.initial_load', 'Seed credit ratings'),
    ('Ba',      ores_utility_system_tenant_id_fn(), 0, 'Ba',      4, 'admin', 'system.initial_load', 'Seed credit ratings'),
    ('B',       ores_utility_system_tenant_id_fn(), 0, 'B',       5, 'admin', 'system.initial_load', 'Seed credit ratings'),
    ('C',       ores_utility_system_tenant_id_fn(), 0, 'C',       6, 'admin', 'system.initial_load', 'Seed credit ratings'),
    ('Default', ores_utility_system_tenant_id_fn(), 0, 'Default', 7, 'admin', 'system.initial_load', 'Seed credit ratings')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Credit Ratings' as entity, count(*) as count
from ores_analytics_credit_ratings_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
