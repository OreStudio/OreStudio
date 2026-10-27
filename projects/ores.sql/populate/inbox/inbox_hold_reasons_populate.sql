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
 * Hold Reasons Population Script
 *
 * Seeds the reasons a decider parks a request with. The list is the
 * operational authorisation queue's: the questions an operator waits on, the
 * handover markers between regional teams the queue doubles as a follow-the-sun
 * workflow for, and the four exception flags that cause a hold. This script is
 * idempotent.
 */

\echo '--- Hold Reasons ---'

insert into ores_inbox_hold_reasons_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'awaiting.margin_query', 0,
     'Awaiting a margin query',
     'The operator waits on a query about the margin the customer holds', 10,
     current_user, current_user, 'system.initial_load', 'Initial population of hold reasons'),
    (ores_utility_system_tenant_id_fn(), 'awaiting.sales_ticket', 0,
     'Awaiting the sales ticket',
     'The operator waits on a question raised on the sales ticket', 20,
     current_user, current_user, 'system.initial_load', 'Initial population of hold reasons'),
    (ores_utility_system_tenant_id_fn(), 'awaiting.amendment', 0,
     'Awaiting an amendment',
     'The operator waits on an amendment to the terms before answering', 30,
     current_user, current_user, 'system.initial_load', 'Initial population of hold reasons'),
    (ores_utility_system_tenant_id_fn(), 'handover.asia_to_authorise', 0,
     'Asia to authorise',
     'The request is handed to Asia, which answers it in its own session', 40,
     current_user, current_user, 'system.initial_load', 'Initial population of hold reasons'),
    (ores_utility_system_tenant_id_fn(), 'handover.london_to_follow', 0,
     'London to follow',
     'The request is handed to London, which follows it up', 50,
     current_user, current_user, 'system.initial_load', 'Initial population of hold reasons'),
    (ores_utility_system_tenant_id_fn(), 'handover.new_york_order', 0,
     'New York order',
     'The request is handed to New York, which answers it in the order it arrived', 60,
     current_user, current_user, 'system.initial_load', 'Initial population of hold reasons'),
    (ores_utility_system_tenant_id_fn(), 'exception.late', 0,
     'Late',
     'The deal was saved or executed past a cut-off', 70,
     current_user, current_user, 'system.initial_load', 'Initial population of hold reasons'),
    (ores_utility_system_tenant_id_fn(), 'exception.off_premises', 0,
     'Off-premises',
     'The deal was executed somewhere other than the trader agreed location', 80,
     current_user, current_user, 'system.initial_load', 'Initial population of hold reasons'),
    (ores_utility_system_tenant_id_fn(), 'exception.out_of_hours', 0,
     'Out of hours',
     'The deal was executed outside regional business hours, at a weekend or on a public holiday', 90,
     current_user, current_user, 'system.initial_load', 'Initial population of hold reasons'),
    (ores_utility_system_tenant_id_fn(), 'exception.unconfirmed', 0,
     'Unconfirmed',
     'The deal is not confirmed, or was confirmed too late', 100,
     current_user, current_user, 'system.initial_load', 'Initial population of hold reasons')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
