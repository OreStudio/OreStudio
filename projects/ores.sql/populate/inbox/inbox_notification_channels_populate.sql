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
 * Notification Channels Population Script
 *
 * Seeds the ways a person is reached by a notification. This script is
 * idempotent.
 */

\echo '--- Notification Channels ---'

insert into ores_inbox_notification_channels_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'in_app', 0, 'In the application',
     'The bell in the header, and live arrival on an open screen', 10,
     current_user, current_user, 'system.initial_load', 'Initial population of notification channels'),
    (ores_utility_system_tenant_id_fn(), 'mail', 0, 'Mail',
     'A message to the account''s address, for what a person must see without signing in', 20,
     current_user, current_user, 'system.initial_load', 'Initial population of notification channels')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
