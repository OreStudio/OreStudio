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
 * Party Types Population Script
 *
 * Populates the classification of legal entities for party and counterparty
 * records.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Party Types ---'

insert into ores_refdata_party_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'Internal', 0, 'Internal',
     'Internal entity for platform administration and organisational purposes. Auto-created system parties use this type; also available for user-created internal entities.',
     0, current_user, current_user, 'system.initial_load', 'Initial population of party types'),
    (ores_iam_system_tenant_id_fn(), 'Bank', 0, 'Bank',
     'A licensed financial institution that accepts deposits, makes loans, and provides other financial services.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of party types'),
    (ores_iam_system_tenant_id_fn(), 'CorporateGroup', 0, 'Corporate Group',
     'A parent entity comprising multiple subsidiaries or divisions operating as a single economic unit.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of party types'),
    (ores_iam_system_tenant_id_fn(), 'HedgeFund', 0, 'Hedge Fund',
     'An alternative investment vehicle using pooled funds and various strategies to earn returns for investors.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of party types'),
    (ores_iam_system_tenant_id_fn(), 'Corporate', 0, 'Corporate',
     'A non-financial corporation that may engage in hedging or treasury operations.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of party types'),
    (ores_iam_system_tenant_id_fn(), 'CentralBank', 0, 'Central Bank',
     'A national monetary authority responsible for monetary policy, currency issuance, and financial system stability.',
     6, current_user, current_user, 'system.initial_load', 'Initial population of party types'),
    (ores_iam_system_tenant_id_fn(), 'Exchange', 0, 'Exchange',
     'A regulated marketplace for trading securities, derivatives, or other financial instruments.',
     7, current_user, current_user, 'system.initial_load', 'Initial population of party types'),
    (ores_iam_system_tenant_id_fn(), 'Individual', 0, 'Individual',
     'A natural person acting as a counterparty in financial transactions.',
     8, current_user, current_user, 'system.initial_load', 'Initial population of party types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_party_types' as entity, count(*) as count
from ores_refdata_party_types_tbl;
