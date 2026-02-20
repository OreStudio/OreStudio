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
 * Trade Lifecycle Events Population Script
 *
 * Seeds the database with valid lifecycle event types.
 * The fsm_state_id column is left null here; it is linked to FSM states
 * in dq_fsm_trade_lifecycle_populate.sql after states are created.
 *
 * This script is idempotent.
 */

\echo '--- Trade Lifecycle Events ---'

insert into ores_trading_lifecycle_events_tbl (
    code, tenant_id, version, description, fsm_state_id,
    modified_by, change_reason_code, change_commentary
) values
    ('New',                ores_iam_system_tenant_id_fn(), 0, 'Initial trade booking', null,
     current_user, 'system.initial_load', 'Seed trade lifecycle events'),
    ('Amendment',          ores_iam_system_tenant_id_fn(), 0, 'Change to trade economics/terms', null,
     current_user, 'system.initial_load', 'Seed trade lifecycle events'),
    ('Novation',           ores_iam_system_tenant_id_fn(), 0, 'Change of counterparty', null,
     current_user, 'system.initial_load', 'Seed trade lifecycle events'),
    ('PartialTermination', ores_iam_system_tenant_id_fn(), 0, 'Reduction of notional', null,
     current_user, 'system.initial_load', 'Seed trade lifecycle events'),
    ('FullTermination',    ores_iam_system_tenant_id_fn(), 0, 'Full termination of the trade', null,
     current_user, 'system.initial_load', 'Seed trade lifecycle events')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Trade Lifecycle Events' as entity, count(*) as count
from ores_trading_lifecycle_events_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
