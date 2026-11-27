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
 * Scheduler Notification Kinds Population Script
 *
 * The three notices housekeeping raises about a store. A component seeds its own
 * kinds beside itself, so these live here and not with the inbox's own four.
 *
 * Two are about a store: it has no bound that applies, or its last sweep failed.
 * A store takes one of them at a time, because the second is a failure a person
 * acts on and the first is a decision nobody made. The third is about a sweep:
 * policies were left unapplied, which is one fact about one sweep and not one
 * fact per policy.
 *
 * The message key is the server's own dotted path, so a screen asks for the key
 * it was sent rather than mapping it. None of the three carries a size, a rate or
 * a period: each links to the page where those are read, and the mail copy is
 * optional for all three, because the in-application copy is always kept.
 *
 * This script is idempotent.
 */

\echo '--- Scheduler Notification Kinds ---'

insert into ores_inbox_notification_kinds_tbl (
    tenant_id, code, version, name, description, message_key, mail_optional,
    display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'scheduler.store_unbounded', 0,
     'Store not bounded',
     'A store the installation keeps has no bound that applies, so nothing removes from it',
     'notification.scheduler.store_unbounded', true, 60,
     current_user, current_user, 'system.initial_load',
     'Initial population of the scheduler notification kinds'),
    (ores_utility_system_tenant_id_fn(), 'scheduler.store_sweep_failed', 0,
     'Sweep failed',
     'The last sweep of a store failed, so the store is unbounded until one succeeds',
     'notification.scheduler.store_sweep_failed', true, 61,
     current_user, current_user, 'system.initial_load',
     'Initial population of the scheduler notification kinds'),
    (ores_utility_system_tenant_id_fn(), 'scheduler.sweep_skipped', 0,
     'Sweep left policies unapplied',
     'A sweep could not apply policies, so the stores they bound are still growing',
     'notification.scheduler.sweep_skipped', true, 62,
     current_user, current_user, 'system.initial_load',
     'Initial population of the scheduler notification kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
