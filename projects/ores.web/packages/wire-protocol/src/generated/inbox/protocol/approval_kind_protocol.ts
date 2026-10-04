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
import type { ApprovalKind } from '../domain/approval_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ApprovalKindKey {
    code: string;
}

export interface ApprovalKindWrite {
    code: string;
    name: string;
    description: string;
    decide_permission_code: string;
    allows_hold: boolean;
    comment_on_approve: boolean;
    expires_after_days: number | null;
    approvals_required: number;
    display_order: number;
}

export interface ApprovalKindChange {
    write: ApprovalKindWrite;
    precondition: Precondition;
}

export interface ApprovalKindRemoval {
    key: ApprovalKindKey;
    precondition: Precondition;
}

export interface ApprovalKindLookup {
    key: ApprovalKindKey;
    approval_kind: ApprovalKind | null;
}

export interface ApprovalKindsFilter {
    code_one_of: string[] | null;
}

export interface ApprovalKindEvent {
    event_id: string;
    key: ApprovalKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ApprovalKindVersionKey {
    approval_kind: ApprovalKindKey;
    version: number;
}

export interface ApprovalKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListApprovalKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalKindsFilter | null;
}

export interface ListApprovalKindsResponse {
    result: Result;
    kinds: ApprovalKind[];
    total: number;
}

export interface GetApprovalKindRequest {
    key: ApprovalKindKey;
}

export interface GetApprovalKindResponse {
    result: Result;
    approval_kind: ApprovalKind | null;
}

export interface GetManyApprovalKindsRequest {
    keys: ApprovalKindKey[];
}

export interface GetManyApprovalKindsResponse {
    result: Result;
    entries: ApprovalKindLookup[];
}

export interface PutApprovalKindRequest {
    change: ApprovalKindChange;
    intent: ChangeIntent;
}

export interface PutApprovalKindResponse {
    result: Result;
    approval_kind: ApprovalKind | null;
}

export interface PutManyApprovalKindsRequest {
    changes: ApprovalKindChange[];
    intent: ChangeIntent;
}

export interface PutManyApprovalKindsResponse {
    result: Result;
    kinds: ApprovalKind[];
}

export interface DeleteApprovalKindRequest {
    removal: ApprovalKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteApprovalKindResponse {
    result: Result;
}

export interface DeleteManyApprovalKindsRequest {
    removals: ApprovalKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyApprovalKindsResponse {
    result: Result;
}

export interface ListApprovalKindVersionsRequest {
    key: ApprovalKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalKindVersionsFilter | null;
}

export interface ListApprovalKindVersionsResponse {
    result: Result;
    versions: ApprovalKind[];
    total: number;
}

export interface GetApprovalKindVersionRequest {
    key: ApprovalKindVersionKey;
}

export interface GetApprovalKindVersionResponse {
    result: Result;
    version: ApprovalKind | null;
}

export const subjects = {
    list_approval_kinds_request: 'inbox.v1.approval_kinds.list',
    get_approval_kind_request: 'inbox.v1.approval_kinds.get',
    get_many_approval_kinds_request: 'inbox.v1.approval_kinds.get_many',
    put_approval_kind_request: 'inbox.v1.approval_kinds.put',
    put_many_approval_kinds_request: 'inbox.v1.approval_kinds.put_many',
    delete_approval_kind_request: 'inbox.v1.approval_kinds.delete',
    delete_many_approval_kinds_request: 'inbox.v1.approval_kinds.delete_many',
    list_approval_kind_versions_request: 'inbox.v1.approval_kinds_versions.list',
    get_approval_kind_version_request: 'inbox.v1.approval_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_approval_kinds_request: true,
    get_approval_kind_request: true,
    get_many_approval_kinds_request: true,
    put_approval_kind_request: true,
    put_many_approval_kinds_request: true,
    delete_approval_kind_request: true,
    delete_many_approval_kinds_request: true,
    list_approval_kind_versions_request: true,
    get_approval_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.approval_kinds_events.created',
    updated: 'inbox.v1.approval_kinds_events.updated',
    deleted: 'inbox.v1.approval_kinds_events.deleted',
} as const;
