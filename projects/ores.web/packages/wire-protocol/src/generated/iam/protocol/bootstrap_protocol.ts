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
export interface BootstrapStatusRequest {}

export interface BootstrapStatusResponse {
    is_in_bootstrap_mode: boolean;
    message: string;
    /**
     * @brief Whether the deployment has a tenant of its own.
     *
     * The system tenant is the deployment's own bookkeeping and not a tenant
     * somebody set up, so it does not count. A deployment without one has not
     * been set up: it has no work in it, and the screen that brings it to life
     * is still the screen a person belongs on, however far through that screen
     * they got before they closed the browser.
     */
    has_tenant: boolean;
    /**
     * @brief The build the answering service runs, in full.
     *
     * The read that reaches a browser before it has a session is the one a screen
     * can state a deployment's version from, and the version belongs to the
     * deployment rather than to the bootstrap question: an interface shows it
     * after the administrator exists as well. A client's own version travels with
     * the client, so the two together say whether the screen in front of somebody
     * came from the build the server is running.
     */
    version: string;
}

export interface CreateInitialAdminRequest {
    principal: string;
    password: string;
    email: string;
}

export interface CreateInitialAdminResponse {
    success: boolean;
    error_message: string;
    account_id: string;
    tenant_name: string;
    tenant_id: string;
}

export const subjects = {
    bootstrap_status_request: 'iam.v1.ops.bootstrap_status',
    create_initial_admin_request: 'iam.v1.ops.create_initial_admin',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    bootstrap_status_request: false,
    create_initial_admin_request: false,
} as const;
