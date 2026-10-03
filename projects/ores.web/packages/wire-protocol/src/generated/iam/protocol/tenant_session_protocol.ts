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
 * @brief Enter one tenant as a system administrator, reading only.
 *
 * Refused to a caller outside the system tenant, to a caller already inside a
 * tenant, to a caller without iam::tenants:impersonate, and for the system
 * tenant, an unknown tenant, or a tenant with no system party yet.
 */
export interface EnterTenantRequest {
    /**
     * @brief The id of the tenant to enter.
     */
    tenant_id: string;
}

export interface EnterTenantResponse {
    success: boolean;
    /**
     * @brief Why the entry was refused, or empty.
     */
    message: string;
    /**
     * @brief The session token scoped to the tenant.
     */
    token: string;
    tenant_id: string;
    tenant_code: string;
    /**
     * @brief The tenant's name, which the screens show while inside.
     */
    tenant_name: string;
    /**
     * @brief The tenant's system party, which the session acts as.
     */
    party_id: string;
    /**
     * @brief The system party's name, which the screens show while inside.
     */
    party_name: string;
    /**
     * @brief How long the session lasts. It is not refreshed.
     */
    access_lifetime_s: number;
}

/**
 * @brief Leave the tenant the caller's session acts in.
 *
 * The tenant comes from the caller's token, so the request carries nothing.
 * Refused to a session that is not inside a tenant.
 */
export interface LeaveTenantRequest {}

export interface LeaveTenantResponse {
    success: boolean;
    message: string;
}

export const subjects = {
    enter_tenant_request: 'iam.v1.ops.enter_tenant',
    leave_tenant_request: 'iam.v1.ops.leave_tenant',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    enter_tenant_request: true,
    leave_tenant_request: true,
} as const;
