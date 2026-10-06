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
 * @brief Records a person's consent that runs of one resource act for them.
 *
 * Sent on behalf of the person, with their token. The grant's tenant and
 * party are the session's, and its grantor is the session's account. Refused
 * when the session acts for no party, or when the person does not hold every
 * permission of the role. A second create for the same resource returns the
 * active grant, and re-activates a revoked one.
 */
export interface CreateRunGrantRequest {
    /**
     * @brief What the grant serves, as <component>.<entity>/<id>.
     */
    resource: string;
    /**
     * @brief The name of the role a run token carries.
     */
    role: string;
    /**
     * @brief The service names that may exchange the grant, comma-separated.
     */
    audience: string;
    /**
     * @brief The number of runs the grant serves; zero for a standing grant.
     */
    max_runs: number;
    /**
     * @brief How long the grant serves runs; zero for a standing grant.
     */
    valid_seconds: number;
}

/**
 * @brief The grant, or why there is none.
 */
export interface CreateRunGrantResponse {
    success: boolean;
    message: string;
    grant_id: string;
    /**
     * @brief True when this request created or re-activated the grant; false
     * when it returned one that was already active.
     */
    created: boolean;
}

/**
 * @brief Ends a run grant.
 *
 * The grantor may revoke their own grant, and a holder of
 * iam::run_grants:revoke may revoke any grant of the tenant. Revoking a
 * revoked grant succeeds and changes nothing.
 */
export interface RevokeRunGrantRequest {
    grant_id: string;
    /**
     * @brief Why the grant ends: unscheduled, deleted, party_changed, or a
     * person's reason.
     */
    reason: string;
}

/**
 * @brief Whether the grant is now revoked.
 */
export interface RevokeRunGrantResponse {
    success: boolean;
    message: string;
}

export const subjects = {
    create_run_grant_request: 'iam.v1.run_grants.create',
    revoke_run_grant_request: 'iam.v1.run_grants.revoke',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    create_run_grant_request: true,
    revoke_run_grant_request: true,
} as const;
