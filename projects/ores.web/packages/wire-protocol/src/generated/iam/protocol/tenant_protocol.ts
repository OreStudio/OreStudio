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
import type { Tenant } from '../domain/tenant.js';

export interface GetTenantsRequest {
    offset: number;
    limit: number;
}

export interface GetTenantsResponse {
    tenants: Tenant[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveTenantRequest {
    data: Tenant;
}

export interface SaveTenantResponse {
    success: boolean;
    message: string;
}

export interface DeleteTenantRequest {
    ids: string[];
}

export interface DeleteTenantResponse {
    success: boolean;
    message: string;
}

export interface GetTenantHistoryRequest {
    id: string;
}

export interface GetTenantHistoryResponse {
    history: Tenant[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_tenants_request: "iam.v1.tenants.list",
    save_tenant_request: "iam.v1.tenants.save",
    delete_tenant_request: "iam.v1.tenants.delete",
    get_tenant_history_request: "iam.v1.tenants.history",
} as const;
