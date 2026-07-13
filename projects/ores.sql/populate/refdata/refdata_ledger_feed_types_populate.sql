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
 * Ledger Feed Types Population Script
 *
 * Populates how a book's ledger balance is fed -- not fed,
 * automatically, or manually -- mutually exclusive with every other
 * feed type.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Ledger Feed Types ---'

insert into ores_refdata_ledger_feed_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'None', 0, 'None',
     'The book''s ledger balance is not fed from any source book -- the default for a book with no ledger connection.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of ledger feed types'),
    (ores_utility_system_tenant_id_fn(), 'Automatic', 0, 'Automatic',
     'The book''s ledger balance is fed by an automated ledger process.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of ledger feed types'),
    (ores_utility_system_tenant_id_fn(), 'Manual', 0, 'Manual',
     'The book''s ledger balance is fed by manual entry.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of ledger feed types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_ledger_feed_types' as entity, count(*) as count
from ores_refdata_ledger_feed_types_tbl;
