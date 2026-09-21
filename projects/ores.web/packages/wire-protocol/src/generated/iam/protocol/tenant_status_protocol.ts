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
import type { TenantStatus } from '../domain/tenant_status.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenantStatusKey {
    status: string;
}

export interface TenantStatusWrite {
    status: string;
    name: string;
    description: string;
    display_order: number;
}

export interface TenantStatusChange {
    write: TenantStatusWrite;
    precondition: Precondition;
}

export interface TenantStatusRemoval {
    key: TenantStatusKey;
    precondition: Precondition;
}

export interface TenantStatusLookup {
    key: TenantStatusKey;
    tenant_status: TenantStatus | null;
}

export interface TenantStatusEvent {
    event_id: string;
    key: TenantStatusKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenantStatusVersionKey {
    tenant_status: TenantStatusKey;
    version: number;
}

export interface TenantStatusVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenantStatusesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTenantStatusesResponse {
    result: Result;
    statuses: TenantStatus[];
    total: number;
}

export interface GetTenantStatusRequest {
    key: TenantStatusKey;
}

export interface GetTenantStatusResponse {
    result: Result;
    tenant_status: TenantStatus | null;
}

export interface GetManyTenantStatusesRequest {
    keys: TenantStatusKey[];
}

export interface GetManyTenantStatusesResponse {
    result: Result;
    entries: TenantStatusLookup[];
}

export interface PutTenantStatusRequest {
    change: TenantStatusChange;
    intent: ChangeIntent;
}

export interface PutTenantStatusResponse {
    result: Result;
    tenant_status: TenantStatus;
}

export interface PutManyTenantStatusesRequest {
    changes: TenantStatusChange[];
    intent: ChangeIntent;
}

export interface PutManyTenantStatusesResponse {
    result: Result;
    statuses: TenantStatus[];
}

export interface DeleteTenantStatusRequest {
    removal: TenantStatusRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenantStatusResponse {
    result: Result;
}

export interface DeleteManyTenantStatusesRequest {
    removals: TenantStatusRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenantStatusesResponse {
    result: Result;
}

export interface ListTenantStatusVersionsRequest {
    key: TenantStatusKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenantStatusVersionsFilter | null;
}

export interface ListTenantStatusVersionsResponse {
    result: Result;
    versions: TenantStatus[];
    total: number;
}

export interface GetTenantStatusVersionRequest {
    key: TenantStatusVersionKey;
}

export interface GetTenantStatusVersionResponse {
    result: Result;
    version: TenantStatus;
}

export const subjects = {
    list_tenant_statuses_request: "iam.v1.tenant_statuses.list",
    get_tenant_status_request: "iam.v1.tenant_statuses.get",
    get_many_tenant_statuses_request: "iam.v1.tenant_statuses.get_many",
    put_tenant_status_request: "iam.v1.tenant_statuses.put",
    put_many_tenant_statuses_request: "iam.v1.tenant_statuses.put_many",
    delete_tenant_status_request: "iam.v1.tenant_statuses.delete",
    delete_many_tenant_statuses_request: "iam.v1.tenant_statuses.delete_many",
    list_tenant_status_versions_request: "iam.v1.tenant_statuses_versions.list",
    get_tenant_status_version_request: "iam.v1.tenant_statuses_versions.get",
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "iam.v1.tenant_statuses_events.created",
    updated: "iam.v1.tenant_statuses_events.updated",
    deleted: "iam.v1.tenant_statuses_events.deleted",
} as const;
