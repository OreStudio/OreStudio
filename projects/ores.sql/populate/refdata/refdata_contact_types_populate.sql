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
 * Contact Types Population Script
 *
 * Populates the classification of contact information purpose for
 * party and counterparty records.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Contact Types ---'

insert into ores_refdata_contact_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'Legal', 0, 'Legal',
     'Registered legal address and contact details for regulatory and compliance purposes.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of contact types'),
    (ores_iam_system_tenant_id_fn(), 'Operations', 0, 'Operations',
     'Day-to-day operational contact details for trade confirmations and general business communication.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of contact types'),
    (ores_iam_system_tenant_id_fn(), 'Settlement', 0, 'Settlement',
     'Contact details for settlement instructions, payment processing, and reconciliation.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of contact types'),
    (ores_iam_system_tenant_id_fn(), 'Billing', 0, 'Billing',
     'Contact details for invoicing, fee collection, and billing-related correspondence.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of contact types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_contact_types' as entity, count(*) as count
from ores_refdata_contact_types_tbl;
