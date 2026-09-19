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
export interface PartySummary {
    id: string;
    name: string;
    party_category: string;
    business_center_code: string;
}

export interface LoginRequest {
    principal: string;
    password: string;
}

export interface LoginResponse {
    success: boolean;
    account_id: string;
    tenant_id: string;
    tenant_name: string;
    username: string;
    email: string;
    password_reset_required: boolean;
    tenant_bootstrap_mode: boolean;
    party_setup_required: boolean;
    /**
     * @brief Set when the party provisioner wizard has completed
     * (onboarding.party = true) but the party is still Inactive. The
     * client should show a message instead of re-launching the wizard.
     */
    party_setup_warning: string;
    token: string;
    error_message: string;
    message: string;
    selected_party_id: string;
    available_parties: PartySummary[];
    /**
     * @brief The account's stored default party, if set and among
     * @c available_parties. Empty when unset. Only meaningful when
     * @c selected_party_id is empty (multi-party login, picker step).
     */
    default_party_id: string;
    /**
     * @brief Token lifetime in seconds as configured on the server.
     *
     * Clients use this to arm the proactive refresh timer so that the
     * timer interval tracks any server-side configuration changes.
     */
    access_lifetime_s: number;
    /**
     * @brief The IAM session UUID created for this login.
     *
     * Matches the session record in ores_iam_sessions_tbl. Clients should
     * forward this as Nats-Session-Id on every subsequent request so that
     * all calls from a single login session can be correlated in logs.
     */
    session_id: string;
}

export interface LogoutRequest {
}

export interface LogoutResponse {
    success: boolean;
    message: string;
}

export interface PublicKeyRequest {
}

/**
 * @brief Request to refresh a JWT token.
 *
 * The current token is passed in the Authorization: Bearer header.
 * No request body is needed — identity is taken from the token claims.
 */
export interface RefreshRequest {
}

/**
 * @brief Response to a token refresh request.
 */
export interface RefreshResponse {
    success: boolean;
    token: string;
    message: string;
    /**
     * @brief Token lifetime in seconds for the newly issued token.
     *
     * Clients re-arm the proactive refresh timer using this value.
     */
    access_lifetime_s: number;
}

/**
 * @brief Authenticates a service account and issues a JWT.
 *
 * Service accounts cannot log in with the regular password-based login path.
 * They authenticate by presenting their database user password (which is
 * stored as a SHA-256 hash in the service account row). On success the IAM
 * service creates a session and returns a short-lived RS256 JWT identical in
 * structure to a human login token.
 *
 * The @p username must match the @c username column of an existing service
 * account (i.e. the database user name such as "ores_local1_reporting_service").
 * The @p password is the plaintext database password for that user.
 */
export interface ServiceLoginRequest {
    username: string;
    password: string;
}

export interface ServiceLoginResponse {
    success: boolean;
    token: string;
    message: string;
    access_lifetime_s: number;
}

export const subjects = {
    login_request: "iam.v1.auth.login",
    logout_request: "iam.v1.auth.logout",
    public_key_request: "iam.v1.auth.public-key",
    refresh_request: "iam.v1.auth.refresh",
    service_login_request: "iam.v1.auth.service-login",
} as const;
