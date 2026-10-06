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
import type { ApprovalRequest } from '../domain/approval_request.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ApprovalRequestKey {
    id: string;
}

export interface ApprovalRequestWrite {
    id: string;
    kind_code: string;
    state_code: string;
    requested_by: string;
    requested_at: string;
    reason: string;
    expires_at: string | null;
}

export interface ApprovalRequestChange {
    write: ApprovalRequestWrite;
    precondition: Precondition;
}

export interface ApprovalRequestRemoval {
    key: ApprovalRequestKey;
    precondition: Precondition;
}

export interface ApprovalRequestLookup {
    key: ApprovalRequestKey;
    approval_request: ApprovalRequest | null;
}

export interface ApprovalRequestsFilter {
    kind_code: string | null;
    state_code: string | null;
    requested_by: string | null;
    id_one_of: string[] | null;
    kind_code_one_of: string[] | null;
    state_code_one_of: string[] | null;
    requested_by_one_of: string[] | null;
}

export interface ApprovalRequestEvent {
    event_id: string;
    key: ApprovalRequestKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ApprovalRequestVersionKey {
    approval_request: ApprovalRequestKey;
    version: number;
}

export interface ApprovalRequestVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListApprovalRequestsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalRequestsFilter | null;
    as_of: string | null;
}

export interface ListApprovalRequestsResponse {
    result: Result;
    requests: ApprovalRequest[];
    total: number;
}

export interface GetApprovalRequestRequest {
    key: ApprovalRequestKey;
}

export interface GetApprovalRequestResponse {
    result: Result;
    approval_request: ApprovalRequest | null;
}

export interface GetManyApprovalRequestsRequest {
    keys: ApprovalRequestKey[];
}

export interface GetManyApprovalRequestsResponse {
    result: Result;
    entries: ApprovalRequestLookup[];
}

export interface PutApprovalRequestRequest {
    change: ApprovalRequestChange;
    intent: ChangeIntent;
}

export interface PutApprovalRequestResponse {
    result: Result;
    approval_request: ApprovalRequest | null;
}

export interface PutManyApprovalRequestsRequest {
    changes: ApprovalRequestChange[];
    intent: ChangeIntent;
}

export interface PutManyApprovalRequestsResponse {
    result: Result;
    requests: ApprovalRequest[];
}

export interface DeleteApprovalRequestRequest {
    removal: ApprovalRequestRemoval;
    intent: ChangeIntent;
}

export interface DeleteApprovalRequestResponse {
    result: Result;
}

export interface DeleteManyApprovalRequestsRequest {
    removals: ApprovalRequestRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyApprovalRequestsResponse {
    result: Result;
}

export interface ListByKindCodeApprovalRequestsRequest {
    kind_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalRequestsFilter | null;
}

export interface ListByKindCodeApprovalRequestsResponse {
    result: Result;
    requests: ApprovalRequest[];
    total: number;
}

export interface ListApprovalRequestVersionsRequest {
    key: ApprovalRequestKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalRequestVersionsFilter | null;
}

export interface ListApprovalRequestVersionsResponse {
    result: Result;
    versions: ApprovalRequest[];
    total: number;
}

export interface GetApprovalRequestVersionRequest {
    key: ApprovalRequestVersionKey;
}

export interface GetApprovalRequestVersionResponse {
    result: Result;
    version: ApprovalRequest | null;
}

export const subjects = {
    list_approval_requests_request: 'inbox.v1.approval_requests.list',
    get_approval_request_request: 'inbox.v1.approval_requests.get',
    get_many_approval_requests_request: 'inbox.v1.approval_requests.get_many',
    put_approval_request_request: 'inbox.v1.approval_requests.put',
    put_many_approval_requests_request: 'inbox.v1.approval_requests.put_many',
    delete_approval_request_request: 'inbox.v1.approval_requests.delete',
    delete_many_approval_requests_request: 'inbox.v1.approval_requests.delete_many',
    list_by_kind_code_approval_requests_request: 'inbox.v1.approval_requests.list_by_kind_code',
    list_approval_request_versions_request: 'inbox.v1.approval_requests_versions.list',
    get_approval_request_version_request: 'inbox.v1.approval_requests_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_approval_requests_request: true,
    get_approval_request_request: true,
    get_many_approval_requests_request: true,
    put_approval_request_request: true,
    put_many_approval_requests_request: true,
    delete_approval_request_request: true,
    delete_many_approval_requests_request: true,
    list_by_kind_code_approval_requests_request: true,
    list_approval_request_versions_request: true,
    get_approval_request_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.approval_requests_events.created',
    updated: 'inbox.v1.approval_requests_events.updated',
    deleted: 'inbox.v1.approval_requests_events.deleted',
} as const;
