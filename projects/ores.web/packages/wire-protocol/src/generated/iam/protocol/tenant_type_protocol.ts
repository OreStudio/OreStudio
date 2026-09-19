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
import type { TenantType } from '../domain/tenant_type.js';

export interface GetTenantTypesRequest {
    offset: number;
    limit: number;
}

export interface GetTenantTypesResponse {
    types: TenantType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveTenantTypeRequest {
    data: TenantType;
}

export interface SaveTenantTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteTenantTypeRequest {
    types: string[];
}

export interface DeleteTenantTypeResponse {
    success: boolean;
    message: string;
}

export interface GetTenantTypeHistoryRequest {
    type: string;
}

export interface GetTenantTypeHistoryResponse {
    history: TenantType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_tenant_types_request: "iam.v1.tenant_types.list",
    save_tenant_type_request: "iam.v1.tenant_types.save",
    delete_tenant_type_request: "iam.v1.tenant_types.delete",
    get_tenant_type_history_request: "iam.v1.tenant_types.history",
} as const;
