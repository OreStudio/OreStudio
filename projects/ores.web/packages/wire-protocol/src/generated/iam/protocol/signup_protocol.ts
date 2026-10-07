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
export interface SignupRequest {
    principal: string;
    password: string;
    email: string;
    /**
     * @brief The address the registration arrived at.
     *
     * The address names the tenant the account lands in: the service resolves
     * it by hostname and uses the tenant flagged @c is_registration_default
     * only when the address names none. Without it a registration lands in the
     * system tenant by omission, which is the defect the survey found.
     */
    hostname: string;
}

export interface SignupResponse {
    success: boolean;
    message: string;
    account_id: string;
    /**
     * @brief The state the account was created in: @c active or @c pending.
     *
     * An account is active when its tenant nominated a default party, so it
     * received an association and can sign in at once. It is pending when the
     * tenant nominated none: the account exists, but it waits for an
     * administrator to finish setting it up.
     */
    account_status: string;
    /**
     * @brief The party the account received, when the tenant nominated one.
     *
     * Empty when the account is pending, because a pending account has no
     * association yet.
     */
    party_id: string;
    /**
     * @brief The role the account received: the tenant's nominated default.
     *
     * Empty when the tenant nominated no role.
     */
    role_id: string;
    /**
     * @brief The stable code a client branches on.
     *
     * Empty when the registration succeeded. The message beside it is for a
     * person and may change; this is what a screen branches on, so a refusal is
     * a value rather than a sentence to match. The codes a registration can
     * answer with are collected on the Entry journeys page.
     */
    error_code: string;
}

export const subjects = {
    signup_request: 'iam.v1.ops.signup',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    signup_request: false,
} as const;
