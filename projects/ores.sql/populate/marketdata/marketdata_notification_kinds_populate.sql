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
 * Market Data Notification Kinds Population Script
 *
 * The two notices the market data service raises about a published curve. A
 * component that raises its own kinds seeds them in its own populate script, so
 * these live here and not with the inbox's own four.
 *
 * A publish tells the people who read the curve that a new version is live. A
 * withdrawal tells them it left use, and carries the reason in words, because a
 * withdrawn curve is a change a reader must act on. Both name the subject and
 * the act and never the values: a notice that carried the points would duplicate
 * the curve and drift from it, and the link it carries is where the values are
 * read.
 *
 * The message key is the server's own dotted path, so a screen asks for the key
 * it was sent rather than mapping it. The mail copy is optional for both: the
 * in-application copy is always kept, and the withdrawal is the one a person is
 * recommended to have mailed.
 *
 * This script is idempotent.
 */

\echo '--- Market Data Notification Kinds ---'

insert into ores_inbox_notification_kinds_tbl (
    tenant_id, code, version, name, description, message_key, mail_optional,
    display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'marketdata.curve_published', 0,
     'Curve published', 'A new version of a curve became the one the desk prices from',
     'notification.marketdata.curve_published', true, 50,
     current_user, current_user, 'system.initial_load',
     'Initial population of the market data notification kinds'),
    (ores_utility_system_tenant_id_fn(), 'marketdata.curve_withdrawn', 0,
     'Curve withdrawn', 'A published curve was taken out of use, with the reason against the act',
     'notification.marketdata.curve_withdrawn', true, 51,
     current_user, current_user, 'system.initial_load',
     'Initial population of the market data notification kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
