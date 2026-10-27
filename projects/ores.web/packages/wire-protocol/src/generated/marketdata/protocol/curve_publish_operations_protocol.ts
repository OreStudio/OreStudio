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
 * @brief Publishes a bootstrapped curve's output under the approval that
 * permits it.
 *
 * The call is the second half of a decision: the person asking raised an
 * approval request of the kind marketdata.curve_publish, somebody else
 * decided it, and this names the approved request. A call that names no
 * approved request publishes nothing, and the answer states the kind the
 * request must be of.
 */
export interface PublishCurveRequest {
    /**
     * @brief The bootstrap configuration whose output is published.
     */
    bootstrap_config_id: string;
    /**
     * @brief The snapshot whose market observations the bootstrap reads.
     */
    as_of: string;
    /**
     * @brief The approved request that permits this publish, or empty.
     *
     * Empty is the ask: the request is raised and nothing is published. Naming an
     * approved request applies it.
     */
    approval_request_id: string;
    /**
     * @brief Why the publish was asked for, which the decider reads.
     *
     * Required when the call raises a request, and ignored when it applies one.
     */
    reason: string;
}

export interface PublishCurveResponse {
    /**
     * @brief Whether the published set moved.
     */
    success: boolean;
    /**
     * @brief Why the publish did not happen, or empty when it did.
     */
    message: string;
    /**
     * @brief The request the answer is about, as it was named or as it stands.
     */
    approval_request_id: string;
    /**
     * @brief The state the request was in, so a caller reads waiting or refused
     * rather than guessing.
     */
    state_code: string;
}

export const subjects = {
    publish_curve_request: 'marketdata.v1.ops.publish_curve',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    publish_curve_request: true,
} as const;
