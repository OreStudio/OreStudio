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
import type { Role } from '../domain/role.js';
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

/**
 * @brief Reads the roles one approval request asks for.
 *
 * Answered when the caller raised the request, and when the caller may read
 * the roles of role grant requests. Answered as not found otherwise, so a
 * caller learns nothing about a request they may not read.
 */
export interface GetRequestRolesRequest {
    /**
     * @brief The approval request to read the roles of, as a UUID string.
     */
    request_id: string;
}

/**
 * @brief One role a request asks for, with the request's own record of it.
 *
 * The role is the catalogue row, whole, so a screen draws it without a second
 * read. The tail is the junction row the ask wrote: when it was written, and
 * when IAM applied it afterwards and on whose authority. It is what the
 * request's story needs to say that a role was given, and the only place that
 * fact is kept.
 */
export interface RequestedRole {
    role: Role;
    /**
     * @brief When the request recorded the role, which is when the person asked.
     */
    asked_at: string;
    /**
     * @brief When IAM applied the role after the request was approved, or empty
     * until it has. A role the person already held is marked applied without a
     * grant, so this says the request was dealt with rather than that a role moved.
     */
    applied_at: string | null;
    /**
     * @brief Who applied it, or empty until somebody has.
     */
    applied_by: string;
}

export interface GetRequestRolesResponse {
    result: Result;
    roles: RequestedRole[];
}

export const subjects = {
    ask_for_roles_request: 'iam.v1.ops.ask_for_roles',
    get_request_roles_request: 'iam.v1.ops.get_request_roles',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    ask_for_roles_request: true,
    get_request_roles_request: true,
} as const;
