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
 * Book Purpose Types Population Script
 *
 * Populates the risk role a book plays -- what kind of trading/
 * operational activity happens in it, mutually exclusive with every
 * other purpose.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Book Purpose Types ---'

insert into ores_refdata_book_purpose_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'Trading', 0, 'Trading',
     'Ordinary trading book: market-making, arbitrage, and client-facing risk-taking. The default purpose for a book that does not play one of the specialised roles below.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of book purpose types'),
    (ores_utility_system_tenant_id_fn(), 'Reserve', 0, 'Reserve',
     'Holds risk retained by the desk rather than passed on -- e.g. residual risk after a hedge, or a deliberate risk-warehousing position.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of book purpose types'),
    (ores_utility_system_tenant_id_fn(), 'Funding', 0, 'Funding',
     'Books the short-term funding/liquidity trades (e.g. the funding process) that keep the desk''s cash and collateral positions square.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of book purpose types'),
    (ores_utility_system_tenant_id_fn(), 'Wash', 0, 'Wash',
     'Routes offsetting back-to-back trades to a designated risk book so client-facing books stay flat; see the paired risk-book link.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of book purpose types'),
    (ores_utility_system_tenant_id_fn(), 'WriteOff', 0, 'Write-off',
     'Holds positions/balances that have been written off and no longer represent live risk, kept for audit trail rather than active management.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of book purpose types'),
    (ores_utility_system_tenant_id_fn(), 'Test', 0, 'Test',
     'Non-production book used for testing booking flows, integrations, or training -- never carries real risk.',
     6, current_user, current_user, 'system.initial_load', 'Initial population of book purpose types'),
    (ores_utility_system_tenant_id_fn(), 'Sales', 0, 'Sales',
     'Books sales-credit or client-relationship-tracking entries that are not themselves risk-bearing trading activity.',
     7, current_user, current_user, 'system.initial_load', 'Initial population of book purpose types'),
    (ores_utility_system_tenant_id_fn(), 'SweepTarget', 0, 'Sweep target',
     'The single central book that receives spot-sweep transfers from sweepable books; owned by Treasury, realises trading P&L. Not combinable with Remittance target.',
     8, current_user, current_user, 'system.initial_load', 'Initial population of book purpose types'),
    (ores_utility_system_tenant_id_fn(), 'RemittanceTarget', 0, 'Remittance target',
     'The single central book that receives central-remittance transfers; owned by Finance, balance-sheet-only (no trading P&L). Not combinable with Sweep target.',
     9, current_user, current_user, 'system.initial_load', 'Initial population of book purpose types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_book_purpose_types' as entity, count(*) as count
from ores_refdata_book_purpose_types_tbl;
