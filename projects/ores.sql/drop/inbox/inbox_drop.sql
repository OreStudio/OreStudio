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

-- =============================================================================
-- Drop Inbox Tables
-- =============================================================================
-- Children first, so nothing is dropped while a reference holds.

\ir ./inbox_notification_preferences_notify_trigger_drop.sql
\ir ./inbox_notification_preferences_drop.sql
\ir ./inbox_notification_deliveries_notify_trigger_drop.sql
\ir ./inbox_notification_deliveries_drop.sql
\ir ./inbox_notification_recipient_drop.sql
\ir ./inbox_notification_argument_drop.sql
\ir ./inbox_notifications_notify_trigger_drop.sql
\ir ./inbox_notifications_drop.sql
\ir ./inbox_delivery_outcome_types_notify_trigger_drop.sql
\ir ./inbox_delivery_outcome_types_drop.sql
\ir ./inbox_notification_channels_notify_trigger_drop.sql
\ir ./inbox_notification_channels_drop.sql
\ir ./inbox_notification_kinds_notify_trigger_drop.sql
\ir ./inbox_notification_kinds_drop.sql
\ir ./inbox_approval_decision_rules_drop.sql
\ir ./inbox_approval_decisions_notify_trigger_drop.sql
\ir ./inbox_approval_decisions_drop.sql
\ir ./inbox_approval_requests_notify_trigger_drop.sql
\ir ./inbox_approval_requests_drop.sql
\ir ./inbox_approval_decision_types_notify_trigger_drop.sql
\ir ./inbox_approval_decision_types_drop.sql
\ir ./inbox_approval_request_states_notify_trigger_drop.sql
\ir ./inbox_approval_request_states_drop.sql
\ir ./inbox_approval_kinds_notify_trigger_drop.sql
\ir ./inbox_approval_kinds_drop.sql
