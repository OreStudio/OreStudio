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
 * Trade Types Population Script
 *
 * Seeds the database with ORE instrument type codes.
 * This script is idempotent.
 */

\echo '--- Trade Types ---'

insert into ores_trade_trade_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Swap',              ores_iam_system_tenant_id_fn(), 0, 'Interest Rate Swap',
     current_user, 'system.initial_load', 'Seed trade types'),
    ('FxForward',        ores_iam_system_tenant_id_fn(), 0, 'FX Forward',
     current_user, 'system.initial_load', 'Seed trade types'),
    ('CapFloor',         ores_iam_system_tenant_id_fn(), 0, 'Cap or Floor',
     current_user, 'system.initial_load', 'Seed trade types'),
    ('Swaption',         ores_iam_system_tenant_id_fn(), 0, 'Swaption',
     current_user, 'system.initial_load', 'Seed trade types'),
    ('FxOption',         ores_iam_system_tenant_id_fn(), 0, 'FX Option',
     current_user, 'system.initial_load', 'Seed trade types'),
    ('CreditDefaultSwap', ores_iam_system_tenant_id_fn(), 0, 'Credit Default Swap',
     current_user, 'system.initial_load', 'Seed trade types'),
    ('Bond',             ores_iam_system_tenant_id_fn(), 0, 'Fixed Income Bond',
     current_user, 'system.initial_load', 'Seed trade types'),
    ('EquityOption',     ores_iam_system_tenant_id_fn(), 0, 'Equity Option',
     current_user, 'system.initial_load', 'Seed trade types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Trade Types' as entity, count(*) as count
from ores_trade_trade_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
