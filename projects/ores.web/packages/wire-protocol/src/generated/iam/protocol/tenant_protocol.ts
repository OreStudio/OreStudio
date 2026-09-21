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
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenantKey {
    id: string;
}

export interface TenantWrite {
    id: string;
    code: string;
    name: string;
    type: string;
    description: string;
    hostname: string;
    status: string;
}

export interface TenantChange {
    write: TenantWrite;
    precondition: Precondition;
}

export interface TenantRemoval {
    key: TenantKey;
    precondition: Precondition;
}

export interface TenantLookup {
    key: TenantKey;
    tenant: Tenant | null;
}

export interface TenantEvent {
    event_id: string;
    key: TenantKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenantVersionKey {
    tenant: TenantKey;
    version: number;
}

export interface TenantVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenantsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTenantsResponse {
    result: Result;
    tenants: Tenant[];
    total: number;
}

export interface GetTenantRequest {
    key: TenantKey;
}

export interface GetTenantResponse {
    result: Result;
    tenant: Tenant | null;
}

export interface GetManyTenantsRequest {
    keys: TenantKey[];
}

export interface GetManyTenantsResponse {
    result: Result;
    entries: TenantLookup[];
}

export interface PutTenantRequest {
    change: TenantChange;
    intent: ChangeIntent;
}

export interface PutTenantResponse {
    result: Result;
    tenant: Tenant;
}

export interface PutManyTenantsRequest {
    changes: TenantChange[];
    intent: ChangeIntent;
}

export interface PutManyTenantsResponse {
    result: Result;
    tenants: Tenant[];
}

export interface DeleteTenantRequest {
    removal: TenantRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenantResponse {
    result: Result;
}

export interface DeleteManyTenantsRequest {
    removals: TenantRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenantsResponse {
    result: Result;
}

export interface ListTenantVersionsRequest {
    key: TenantKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenantVersionsFilter | null;
}

export interface ListTenantVersionsResponse {
    result: Result;
    versions: Tenant[];
    total: number;
}

export interface GetTenantVersionRequest {
    key: TenantVersionKey;
}

export interface GetTenantVersionResponse {
    result: Result;
    version: Tenant;
}

export const subjects = {
    list_tenants_request: "iam.v1.tenants.list",
    get_tenant_request: "iam.v1.tenants.get",
    get_many_tenants_request: "iam.v1.tenants.get_many",
    put_tenant_request: "iam.v1.tenants.put",
    put_many_tenants_request: "iam.v1.tenants.put_many",
    delete_tenant_request: "iam.v1.tenants.delete",
    delete_many_tenants_request: "iam.v1.tenants.delete_many",
    list_tenant_versions_request: "iam.v1.tenants_versions.list",
    get_tenant_version_request: "iam.v1.tenants_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenants_request: true,
    get_tenant_request: true,
    get_many_tenants_request: true,
    put_tenant_request: true,
    put_many_tenants_request: true,
    delete_tenant_request: true,
    delete_many_tenants_request: true,
    list_tenant_versions_request: true,
    get_tenant_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "iam.v1.tenants_events.created",
    updated: "iam.v1.tenants_events.updated",
    deleted: "iam.v1.tenants_events.deleted",
} as const;
