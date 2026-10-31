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
 * Draft Reason Population Script
 *
 * Seeds why a trade is a draft and which desk unblocks it. A draft is a deal
 * the desk has captured but not concluded, so the reasons are the things it
 * waits on: the rest of a structure, a confirmation, or credit. The desk is
 * stated because an aged draft with no owner is nobody's work.
 *
 * This script is idempotent.
 */

\echo '--- Draft Reason ---'

insert into ores_trading_draft_reasons_tbl (
    code, tenant_id, version, description, responsible_role,
    modified_by, change_reason_code, change_commentary
) values
    ('structure_incomplete', ores_utility_system_tenant_id_fn(), 0,
     'The deal is a leg of a structure that is not complete, so it cannot go live alone',
     'trading_front_office',
     current_user, 'system.initial_load', 'Seed draft_reason'),
    ('confirmation_pending', ores_utility_system_tenant_id_fn(), 0,
     'The deal waits on a confirmation from the counterparty',
     'trading_middle_office',
     current_user, 'system.initial_load', 'Seed draft_reason'),
    ('credit_review', ores_utility_system_tenant_id_fn(), 0,
     'The deal waits on a credit decision on the counterparty',
     'credit_risk',
     current_user, 'system.initial_load', 'Seed draft_reason'),
    ('pricing_pending', ores_utility_system_tenant_id_fn(), 0,
     'The deal waits on a price both sides agree',
     'trading_front_office',
     current_user, 'system.initial_load', 'Seed draft_reason')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Draft Reason' as entity, count(*) as count
from ores_trading_draft_reasons_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
