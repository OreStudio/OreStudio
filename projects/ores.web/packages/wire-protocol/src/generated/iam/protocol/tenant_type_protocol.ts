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
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenantTypeKey {
    type: string;
}

export interface TenantTypeWrite {
    type: string;
    name: string;
    description: string;
    display_order: number;
}

export interface TenantTypeChange {
    write: TenantTypeWrite;
    precondition: Precondition;
}

export interface TenantTypeRemoval {
    key: TenantTypeKey;
    precondition: Precondition;
}

export interface TenantTypeLookup {
    key: TenantTypeKey;
    tenant_type: TenantType | null;
}

export interface TenantTypeVersionKey {
    tenant_type: TenantTypeKey;
    version: number;
}

export interface TenantTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenantTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTenantTypesResponse {
    result: Result;
    types: TenantType[];
    total: number;
}

export interface GetTenantTypeRequest {
    key: TenantTypeKey;
}

export interface GetTenantTypeResponse {
    result: Result;
    tenant_type: TenantType | null;
}

export interface GetManyTenantTypesRequest {
    keys: TenantTypeKey[];
}

export interface GetManyTenantTypesResponse {
    result: Result;
    entries: TenantTypeLookup[];
}

export interface PutTenantTypeRequest {
    change: TenantTypeChange;
    intent: ChangeIntent;
}

export interface PutTenantTypeResponse {
    result: Result;
    tenant_type: TenantType;
}

export interface PutManyTenantTypesRequest {
    changes: TenantTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyTenantTypesResponse {
    result: Result;
    types: TenantType[];
}

export interface DeleteTenantTypeRequest {
    removal: TenantTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenantTypeResponse {
    result: Result;
}

export interface DeleteManyTenantTypesRequest {
    removals: TenantTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenantTypesResponse {
    result: Result;
}

export interface ListTenantTypeVersionsRequest {
    key: TenantTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenantTypeVersionsFilter | null;
}

export interface ListTenantTypeVersionsResponse {
    result: Result;
    versions: TenantType[];
    total: number;
}

export interface GetTenantTypeVersionRequest {
    key: TenantTypeVersionKey;
}

export interface GetTenantTypeVersionResponse {
    result: Result;
    version: TenantType;
}

export const subjects = {
    list_tenant_types_request: "iam.v1.tenant_types.list",
    get_tenant_type_request: "iam.v1.tenant_types.get",
    get_many_tenant_types_request: "iam.v1.tenant_types.get_many",
    put_tenant_type_request: "iam.v1.tenant_types.put",
    put_many_tenant_types_request: "iam.v1.tenant_types.put_many",
    delete_tenant_type_request: "iam.v1.tenant_types.delete",
    delete_many_tenant_types_request: "iam.v1.tenant_types.delete_many",
    list_tenant_type_versions_request: "iam.v1.tenant_types_versions.list",
    get_tenant_type_version_request: "iam.v1.tenant_types_versions.get",
} as const;
