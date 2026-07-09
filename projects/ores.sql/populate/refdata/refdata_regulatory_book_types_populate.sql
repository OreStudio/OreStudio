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
 * Regulatory Book Types Population Script
 *
 * Populates the Basel III/IV FRTB trading book / banking book
 * classification a book carries.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Regulatory Book Types ---'

insert into ores_refdata_regulatory_book_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'Trading', 0, 'Trading Book',
     'Held with trading intent (market-making, arbitrage, hedging other trading book positions). Marked to market daily; subject to FRTB market risk capital.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of regulatory book types'),
    (ores_utility_system_tenant_id_fn(), 'Banking', 0, 'Banking Book',
     'Held to maturity or for balance-sheet management. Accrual-accounted; subject to credit risk capital (plus a market risk charge on FX/commodity exposure).',
     2, current_user, current_user, 'system.initial_load', 'Initial population of regulatory book types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_regulatory_book_types' as entity, count(*) as count
from ores_refdata_regulatory_book_types_tbl;
