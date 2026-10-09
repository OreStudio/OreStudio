/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief One authentication event, as the log records it.
 *
 * The timestamps are ISO 8601 strings: that is the storage form, and the
 * screen reads them as text rather than as an instant.
 */
export interface AuthEvent {
    id: string;
    event_time: string;
    account_id: string;
    event_type: string;
    username: string;
    session_id: string;
    party_id: string;
    error_detail: string;
}

/**
 * @brief A window over the caller's tenant authentication events.
 *
 * An empty filter does not filter. The window is newest first, so the
 * screen reads the most recent events without asking for an order.
 */
export interface ListAuthEventsRequest {
    account_id: string;
    event_type: string;
    from_time: string;
    to_time: string;
    limit: number;
    offset: number;
}

export interface ListAuthEventsResponse {
    events: AuthEvent[];
    success: boolean;
    message: string;
}

export const subjects = {
    list_auth_events_request: 'iam.v1.auth_events.list',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_auth_events_request: true,
} as const;
