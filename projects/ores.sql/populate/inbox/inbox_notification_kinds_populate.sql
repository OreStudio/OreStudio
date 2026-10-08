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
 * Notification Kinds Population Script
 *
 * Seeds the kinds of notification the inbox raises itself: a request waits, a
 * request is decided, a request is about to run out, and a request ran out of
 * time. An expiry is its own kind rather than a decision, because nobody
 * decided it and a message that named a decider would be telling the member
 * something untrue; an expiring request is its own kind again, because it goes
 * to the deciders rather than to the person who asked, and a message that said
 * the member had been told something would be telling them something untrue.
 * A component that raises its own kinds seeds them in its own populate script.
 * This script is idempotent.
 */

\echo '--- Notification Kinds ---'

insert into ores_inbox_notification_kinds_tbl (
    tenant_id, code, version, name, description, message_key, mail_optional,
    display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'inbox.approval_waiting', 0,
     'Request waiting', 'A request waits for a person who may decide it',
     'notification.inbox.approval_waiting', true, 10,
     current_user, current_user, 'system.initial_load', 'Initial population of notification kinds'),
    (ores_utility_system_tenant_id_fn(), 'inbox.approval_decided', 0,
     'Request decided', 'A request the person asked for was decided',
     'notification.inbox.approval_decided', true, 20,
     current_user, current_user, 'system.initial_load', 'Initial population of notification kinds'),
    (ores_utility_system_tenant_id_fn(), 'inbox.approval_expired', 0,
     'Request expired', 'A request the person asked for ran out of time undecided',
     'notification.inbox.approval_expired', true, 30,
     current_user, current_user, 'system.initial_load', 'Initial population of notification kinds'),
    (ores_utility_system_tenant_id_fn(), 'inbox.approval_expiring', 0,
     'Request about to expire', 'A request waiting in the queue is close to its deadline',
     'notification.inbox.approval_expiring', true, 40,
     current_user, current_user, 'system.initial_load', 'Initial population of notification kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
