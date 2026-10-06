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
import type { RoleGrantRequest } from '../domain/role_grant_request.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface RoleGrantRequestKey {
    request_id: string;
}

export interface RoleGrantRequestWrite {
    request_id: string;
    account_id: string;
}

export interface RoleGrantRequestChange {
    write: RoleGrantRequestWrite;
    precondition: Precondition;
}

export interface RoleGrantRequestRemoval {
    key: RoleGrantRequestKey;
    precondition: Precondition;
}

export interface RoleGrantRequestLookup {
    key: RoleGrantRequestKey;
    role_grant_request: RoleGrantRequest | null;
}

export interface RoleGrantRequestsFilter {
    account_id: string | null;
    request_id_one_of: string[] | null;
    account_id_one_of: string[] | null;
}

export interface RoleGrantRequestEvent {
    event_id: string;
    key: RoleGrantRequestKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RoleGrantRequestVersionKey {
    role_grant_request: RoleGrantRequestKey;
    version: number;
}

export interface RoleGrantRequestVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRoleGrantRequestsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RoleGrantRequestsFilter | null;
}

export interface ListRoleGrantRequestsResponse {
    result: Result;
    role_grant_requests: RoleGrantRequest[];
    total: number;
}

export interface GetRoleGrantRequestRequest {
    key: RoleGrantRequestKey;
}

export interface GetRoleGrantRequestResponse {
    result: Result;
    role_grant_request: RoleGrantRequest | null;
}

export interface GetManyRoleGrantRequestsRequest {
    keys: RoleGrantRequestKey[];
}

export interface GetManyRoleGrantRequestsResponse {
    result: Result;
    entries: RoleGrantRequestLookup[];
}

export interface PutRoleGrantRequestRequest {
    change: RoleGrantRequestChange;
    intent: ChangeIntent;
}

export interface PutRoleGrantRequestResponse {
    result: Result;
    role_grant_request: RoleGrantRequest | null;
}

export interface PutManyRoleGrantRequestsRequest {
    changes: RoleGrantRequestChange[];
    intent: ChangeIntent;
}

export interface PutManyRoleGrantRequestsResponse {
    result: Result;
    role_grant_requests: RoleGrantRequest[];
}

export interface DeleteRoleGrantRequestRequest {
    removal: RoleGrantRequestRemoval;
    intent: ChangeIntent;
}

export interface DeleteRoleGrantRequestResponse {
    result: Result;
}

export interface DeleteManyRoleGrantRequestsRequest {
    removals: RoleGrantRequestRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRoleGrantRequestsResponse {
    result: Result;
}

export interface ListByAccountIdRoleGrantRequestsRequest {
    account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: RoleGrantRequestsFilter | null;
}

export interface ListByAccountIdRoleGrantRequestsResponse {
    result: Result;
    role_grant_requests: RoleGrantRequest[];
    total: number;
}

export interface ListRoleGrantRequestVersionsRequest {
    key: RoleGrantRequestKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RoleGrantRequestVersionsFilter | null;
}

export interface ListRoleGrantRequestVersionsResponse {
    result: Result;
    versions: RoleGrantRequest[];
    total: number;
}

export interface GetRoleGrantRequestVersionRequest {
    key: RoleGrantRequestVersionKey;
}

export interface GetRoleGrantRequestVersionResponse {
    result: Result;
    version: RoleGrantRequest | null;
}

export const subjects = {
    list_role_grant_requests_request: 'iam.v1.role_grant_requests.list',
    get_role_grant_request_request: 'iam.v1.role_grant_requests.get',
    get_many_role_grant_requests_request: 'iam.v1.role_grant_requests.get_many',
    put_role_grant_request_request: 'iam.v1.role_grant_requests.put',
    put_many_role_grant_requests_request: 'iam.v1.role_grant_requests.put_many',
    delete_role_grant_request_request: 'iam.v1.role_grant_requests.delete',
    delete_many_role_grant_requests_request: 'iam.v1.role_grant_requests.delete_many',
    list_by_account_id_role_grant_requests_request: 'iam.v1.role_grant_requests.list_by_account_id',
    list_role_grant_request_versions_request: 'iam.v1.role_grant_requests_versions.list',
    get_role_grant_request_version_request: 'iam.v1.role_grant_requests_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_role_grant_requests_request: true,
    get_role_grant_request_request: true,
    get_many_role_grant_requests_request: true,
    put_role_grant_request_request: true,
    put_many_role_grant_requests_request: true,
    delete_role_grant_request_request: true,
    delete_many_role_grant_requests_request: true,
    list_by_account_id_role_grant_requests_request: true,
    list_role_grant_request_versions_request: true,
    get_role_grant_request_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.role_grant_requests_events.created',
    updated: 'iam.v1.role_grant_requests_events.updated',
    deleted: 'iam.v1.role_grant_requests_events.deleted',
} as const;
