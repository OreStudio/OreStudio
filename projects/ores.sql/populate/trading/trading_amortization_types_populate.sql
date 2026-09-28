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
 * Amortization Type Population Script
 *
 * Seeds the closed bond leg amortization set. The rows are the ORE
 * amortizationType simple type under external/ore/xsd/ore_types.xsd.
 *
 * This script is idempotent.
 */

\echo '--- Amortization Type ---'

insert into ores_trading_amortization_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('FixedAmount', ores_utility_system_tenant_id_fn(), 0, 'A fixed amount each period',
     current_user, 'system.initial_load', 'Seed amortization_type'),
    ('RelativeToInitialNotional', ores_utility_system_tenant_id_fn(), 0, 'A fraction of the initial notional',
     current_user, 'system.initial_load', 'Seed amortization_type'),
    ('RelativeToPreviousNotional', ores_utility_system_tenant_id_fn(), 0, 'A fraction of the previous notional',
     current_user, 'system.initial_load', 'Seed amortization_type'),
    ('Annuity', ores_utility_system_tenant_id_fn(), 0, 'An annuity schedule',
     current_user, 'system.initial_load', 'Seed amortization_type'),
    ('LinearToMaturity', ores_utility_system_tenant_id_fn(), 0, 'A linear run-off to maturity',
     current_user, 'system.initial_load', 'Seed amortization_type')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Amortization Type' as entity, count(*) as count
from ores_trading_amortization_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
