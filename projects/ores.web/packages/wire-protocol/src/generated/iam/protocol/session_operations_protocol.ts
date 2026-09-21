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
import type { Session } from '../domain/session.js';

/**
 * @brief A session with its party-scoped context.
 *
 * The session is the entity; party_id, visible_party_ids and username are
 * the denormalised fields reached through the account-party association.
 * They are message fields because no column backs them.
 */
export interface SessionView {
    session: Session;
    party_id: string;
    visible_party_ids: string[];
    username: string;
}

export interface GetActiveSessionsRequest {
}

export interface GetActiveSessionsResponse {
    sessions: Session[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_active_sessions_request: "iam.v1.sessions.active",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_active_sessions_request: true,
} as const;
