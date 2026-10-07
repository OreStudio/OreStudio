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
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief Asks for roles for the signed-in person.
 *
 * Refused when a role is unknown, is not one the tenant offers to its
 * members, is already held, or is already asked for in a request that still
 * waits.
 */
export interface AskForRolesRequest {
    /**
     * @brief The roles asked for, as UUID strings.
     */
    role_ids: string[];
    /**
     * @brief Why the person asks, in their words.
     */
    reason: string;
}

export interface AskForRolesResponse {
    result: Result;
    /**
     * @brief The approval request raised, when the outcome is ok.
     */
    request_id: string;
}

export const subjects = {
    ask_for_roles_request: 'iam.v1.role-requests.ask',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    ask_for_roles_request: true,
} as const;
