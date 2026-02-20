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
 * Trade Identifier Types Population Script
 *
 * Seeds the database with identifier type codes for trade identifiers.
 * This script is idempotent.
 */

\echo '--- Trade Identifier Types ---'

insert into ores_trading_trade_id_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('UTI',      ores_iam_system_tenant_id_fn(), 0, 'Unique Transaction Identifier',
     current_user, 'system.initial_load', 'Seed trade identifier types'),
    ('USI',      ores_iam_system_tenant_id_fn(), 0, 'Unique Swap Identifier',
     current_user, 'system.initial_load', 'Seed trade identifier types'),
    ('Internal', ores_iam_system_tenant_id_fn(), 0, 'Internal system identifier',
     current_user, 'system.initial_load', 'Seed trade identifier types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Trade Identifier Types' as entity, count(*) as count
from ores_trading_trade_id_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
