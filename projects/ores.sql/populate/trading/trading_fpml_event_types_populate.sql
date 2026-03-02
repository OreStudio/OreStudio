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
 * FpML Event Types Population Script
 *
 * Seeds the database with standard FpML lc_EventType_Type values.
 * These are the five standard FpML event codes used on the wire.
 *
 * This script is idempotent.
 */

\echo '--- FpML Event Types ---'

insert into ores_trading_fpml_event_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('New',
     ores_iam_system_tenant_id_fn(), 0,
     'Initial booking of a new trade.',
     current_user, 'system.initial_load', 'Seed FpML event types'),
    ('Amendment',
     ores_iam_system_tenant_id_fn(), 0,
     'Change to trade economics or terms.',
     current_user, 'system.initial_load', 'Seed FpML event types'),
    ('Novation',
     ores_iam_system_tenant_id_fn(), 0,
     'Transfer of a trade to a new counterparty.',
     current_user, 'system.initial_load', 'Seed FpML event types'),
    ('PartialTermination',
     ores_iam_system_tenant_id_fn(), 0,
     'Reduction of notional on an existing trade.',
     current_user, 'system.initial_load', 'Seed FpML event types'),
    ('FullTermination',
     ores_iam_system_tenant_id_fn(), 0,
     'Full termination of a trade prior to maturity.',
     current_user, 'system.initial_load', 'Seed FpML event types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'FpML Event Types' as entity, count(*) as count
from ores_trading_fpml_event_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
