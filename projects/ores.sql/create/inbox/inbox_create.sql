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
-- Inbox Tables
-- =============================================================================
-- The things that wait for a person: approval requests, decided by many
-- decisions. The lookups come first, because the request and decision insert
-- triggers validate against them. Requests and decisions reference IAM
-- accounts, so this aggregate runs after iam_create.sql.

-- Lookups held by the system tenant
\ir ./inbox_approval_kinds_create.sql
\ir ./inbox_approval_kinds_notify_trigger_create.sql
\ir ./inbox_approval_request_states_create.sql
\ir ./inbox_approval_request_states_notify_trigger_create.sql
\ir ./inbox_approval_decision_types_create.sql
\ir ./inbox_approval_decision_types_notify_trigger_create.sql

-- The request header every kind shares
\ir ./inbox_approval_requests_create.sql
\ir ./inbox_approval_requests_notify_trigger_create.sql

-- The decisions on a request, and the rules every decision keeps
\ir ./inbox_approval_decisions_create.sql
\ir ./inbox_approval_decisions_notify_trigger_create.sql
\ir ./inbox_approval_decision_rules_create.sql
\ir ./inbox_approval_decide_fn_create.sql
\ir ./inbox_approval_expire_fn_create.sql

-- =============================================================================
-- Notifications
-- =============================================================================
-- The things a person is told. The lookups come first: the kinds and channels
-- are held by the system tenant, and the delivery outcomes are a closed set
-- with no tenant that a delivery references with a database foreign key.

\ir ./inbox_notification_kinds_create.sql
\ir ./inbox_notification_kinds_notify_trigger_create.sql
\ir ./inbox_notification_channels_create.sql
\ir ./inbox_notification_channels_notify_trigger_create.sql
\ir ./inbox_delivery_outcome_types_create.sql
\ir ./inbox_delivery_outcome_types_notify_trigger_create.sql

-- The notification, the values its message names, and who receives it
\ir ./inbox_notifications_create.sql
\ir ./inbox_notifications_notify_trigger_create.sql
\ir ./inbox_notification_argument_create.sql
\ir ./inbox_notification_recipient_create.sql

-- Each attempt to reach a recipient, and each person's choices
\ir ./inbox_notification_deliveries_create.sql
\ir ./inbox_notification_deliveries_notify_trigger_create.sql
\ir ./inbox_notification_preferences_create.sql
\ir ./inbox_notification_preferences_notify_trigger_create.sql

-- Raising a notification and a person's read state, each in one statement
\ir ./inbox_notification_fn_create.sql
