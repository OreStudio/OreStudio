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

/**
 * @brief The database row the deployment was built against.
 *
 * The four values =ores_database_info_tbl= records when the database is
 * created or recreated: the hash of the schema it was built from, the build
 * environment, the commit, and when the database was created. The table holds
 * exactly one row. It is declared by hand and owes nothing to the generators,
 * because it records the checkout that built the database before any service
 * runs.
 */
export interface DatabaseInfo {
    /** @brief The hash of the SQL scripts the database was built from. */
    fingerprint: string;
    /** @brief The build environment the database was built in. */
    environment: string;
    /** @brief The git commit the database was built from. */
    commit: string;
    /** @brief When the database was created or recreated. */
    created: string;
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
    /**
     * @brief The build the answering service runs, in full.
     *
     * A session is a session with a deployment, so the answer that opens one
     * states which build it was opened against: a client that signs in states the
     * deployment's version without having asked whether the deployment still
     * needs an administrator, and a client that did ask can see whether the two
     * answers agree.
     */
    version: string;
    /**
     * @brief The database the deployment stores into, in full.
     *
     * The row travels with the build the answer already states, because the two
     * answer one question — what am I talking to — and the database is its third.
     * Its audience is the build's audience: everyone who may open a session may
     * read it, so it needs no subject, no permission and no read of its own. The
     * iam service reads it once per login from =ores_database_info_tbl=.
     */
    database: DatabaseInfo;
    username: string;
    email: string;
    password_reset_required: boolean;
    tenant_bootstrap_mode: boolean;
    /**
     * @brief Whether the caller's tenant is still being set up.
     *
     * This is the tenant's own lifecycle status, which reads @c bootstrapping
     * until the tenant's setup run marks it @c active. It is a fact about the
     * tenant row, so every party of the tenant reads it the same way, and it is
     * what a screen must hold a tenant administrator's setup on.
     *
     * It is stated beside @c tenant_bootstrap_mode rather than derived from it.
     * That flag is a setting under the tenant's system party, and the completing
     * step clears it as a warning rather than a condition: a tenant that is
     * already active can still report the flag as set, so the flag cannot answer
     * this question on its own.
     */
    tenant_bootstrapping: boolean;
    party_setup_required: boolean;
    /**
     * @brief Set when the party provisioner wizard has completed
     * (onboarding.party = true) but the party is still Inactive. The
     * client should show a message instead of re-launching the wizard.
     */
    party_setup_warning: string;
    token: string;
    error_message: string;
    /**
     * @brief The stable code a client branches on.
     *
     * Empty when the sign-in succeeded. The message beside it is for a person
     * and may change; this is what a screen branches on, so a locked account is
     * distinguishable from a wrong password without matching English prose. The
     * codes a sign-in can answer with are collected on the Entry journeys page.
     */
    error_code: string;
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

export interface LogoutRequest {}

export interface LogoutResponse {
    success: boolean;
    message: string;
}

export interface PublicKeyRequest {}

/**
 * @brief Request to refresh a JWT token.
 *
 * The current token is passed in the Authorization: Bearer header.
 * No request body is needed — identity is taken from the token claims.
 */
export interface RefreshRequest {}

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
    login_request: 'iam.v1.ops.login',
    logout_request: 'iam.v1.ops.logout',
    public_key_request: 'iam.v1.ops.public_key',
    refresh_request: 'iam.v1.ops.refresh',
    service_login_request: 'iam.v1.ops.service_login',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    login_request: false,
    logout_request: true,
    public_key_request: false,
    refresh_request: true,
    service_login_request: false,
} as const;
