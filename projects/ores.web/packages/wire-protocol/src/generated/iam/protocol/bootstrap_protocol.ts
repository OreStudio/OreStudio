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
export interface BootstrapStatusRequest {
}

export interface BootstrapStatusResponse {
    is_in_bootstrap_mode: boolean;
    message: string;
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

export interface ProvisionTenantRequest {
    /*
     * tenant type (e.g., "corporate")
     */
    type: string;
    /*
     * unique tenant code
     */
    code: string;
    /*
     * display name
     */
    name: string;
    /*
     * unique hostname
     */
    hostname: string;
    /*
     * optional description
     */
    description: string;
    /*
     * username for the admin account
     */
    principal: string;
    password: string;
    email: string;
}

export interface ProvisionTenantResponse {
    success: boolean;
    error_message: string;
    account_id: string;
    tenant_id: string;
}

export const subjects = {
    bootstrap_status_request: "iam.v1.bootstrap.status",
    create_initial_admin_request: "iam.v1.bootstrap.create-admin",
    provision_tenant_request: "iam.v1.bootstrap.provision-tenant",
} as const;
