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
 * Trade Party Role Types Population Script
 *
 * Seeds the database with FpML-aligned party role codes.
 * This script is idempotent.
 */

\echo '--- Trade Party Role Types ---'

insert into ores_trade_party_role_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Counterparty',       ores_iam_system_tenant_id_fn(), 0, 'The other side of the trade',
     current_user, 'system.initial_load', 'Seed trade party role types'),
    ('CalculationAgent',   ores_iam_system_tenant_id_fn(), 0, 'Determines fixings and payments',
     current_user, 'system.initial_load', 'Seed trade party role types'),
    ('ExecutingBroker',    ores_iam_system_tenant_id_fn(), 0, 'Broker executing the trade',
     current_user, 'system.initial_load', 'Seed trade party role types'),
    ('NovationTransferee', ores_iam_system_tenant_id_fn(), 0, 'New counterparty in a novation',
     current_user, 'system.initial_load', 'Seed trade party role types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Trade Party Role Types' as entity, count(*) as count
from ores_trade_party_role_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
