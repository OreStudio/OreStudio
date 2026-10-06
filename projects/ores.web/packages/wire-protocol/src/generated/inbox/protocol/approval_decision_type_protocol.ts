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
import type { ApprovalDecisionType } from '../domain/approval_decision_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ApprovalDecisionTypeKey {
    code: string;
}

export interface ApprovalDecisionTypeWrite {
    code: string;
    name: string;
    description: string;
    requires_comment: boolean;
    display_order: number;
}

export interface ApprovalDecisionTypeChange {
    write: ApprovalDecisionTypeWrite;
    precondition: Precondition;
}

export interface ApprovalDecisionTypeRemoval {
    key: ApprovalDecisionTypeKey;
    precondition: Precondition;
}

export interface ApprovalDecisionTypeLookup {
    key: ApprovalDecisionTypeKey;
    approval_decision_type: ApprovalDecisionType | null;
}

export interface ApprovalDecisionTypesFilter {
    code_one_of: string[] | null;
}

export interface ApprovalDecisionTypeEvent {
    event_id: string;
    key: ApprovalDecisionTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ApprovalDecisionTypeVersionKey {
    approval_decision_type: ApprovalDecisionTypeKey;
    version: number;
}

export interface ApprovalDecisionTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListApprovalDecisionTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalDecisionTypesFilter | null;
    as_of: string | null;
}

export interface ListApprovalDecisionTypesResponse {
    result: Result;
    decision_types: ApprovalDecisionType[];
    total: number;
}

export interface GetApprovalDecisionTypeRequest {
    key: ApprovalDecisionTypeKey;
}

export interface GetApprovalDecisionTypeResponse {
    result: Result;
    approval_decision_type: ApprovalDecisionType | null;
}

export interface GetManyApprovalDecisionTypesRequest {
    keys: ApprovalDecisionTypeKey[];
}

export interface GetManyApprovalDecisionTypesResponse {
    result: Result;
    entries: ApprovalDecisionTypeLookup[];
}

export interface PutApprovalDecisionTypeRequest {
    change: ApprovalDecisionTypeChange;
    intent: ChangeIntent;
}

export interface PutApprovalDecisionTypeResponse {
    result: Result;
    approval_decision_type: ApprovalDecisionType | null;
}

export interface PutManyApprovalDecisionTypesRequest {
    changes: ApprovalDecisionTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyApprovalDecisionTypesResponse {
    result: Result;
    decision_types: ApprovalDecisionType[];
}

export interface DeleteApprovalDecisionTypeRequest {
    removal: ApprovalDecisionTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteApprovalDecisionTypeResponse {
    result: Result;
}

export interface DeleteManyApprovalDecisionTypesRequest {
    removals: ApprovalDecisionTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyApprovalDecisionTypesResponse {
    result: Result;
}

export interface ListApprovalDecisionTypeVersionsRequest {
    key: ApprovalDecisionTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalDecisionTypeVersionsFilter | null;
}

export interface ListApprovalDecisionTypeVersionsResponse {
    result: Result;
    versions: ApprovalDecisionType[];
    total: number;
}

export interface GetApprovalDecisionTypeVersionRequest {
    key: ApprovalDecisionTypeVersionKey;
}

export interface GetApprovalDecisionTypeVersionResponse {
    result: Result;
    version: ApprovalDecisionType | null;
}

export const subjects = {
    list_approval_decision_types_request: 'inbox.v1.approval_decision_types.list',
    get_approval_decision_type_request: 'inbox.v1.approval_decision_types.get',
    get_many_approval_decision_types_request: 'inbox.v1.approval_decision_types.get_many',
    put_approval_decision_type_request: 'inbox.v1.approval_decision_types.put',
    put_many_approval_decision_types_request: 'inbox.v1.approval_decision_types.put_many',
    delete_approval_decision_type_request: 'inbox.v1.approval_decision_types.delete',
    delete_many_approval_decision_types_request: 'inbox.v1.approval_decision_types.delete_many',
    list_approval_decision_type_versions_request: 'inbox.v1.approval_decision_types_versions.list',
    get_approval_decision_type_version_request: 'inbox.v1.approval_decision_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_approval_decision_types_request: true,
    get_approval_decision_type_request: true,
    get_many_approval_decision_types_request: true,
    put_approval_decision_type_request: true,
    put_many_approval_decision_types_request: true,
    delete_approval_decision_type_request: true,
    delete_many_approval_decision_types_request: true,
    list_approval_decision_type_versions_request: true,
    get_approval_decision_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.approval_decision_types_events.created',
    updated: 'inbox.v1.approval_decision_types_events.updated',
    deleted: 'inbox.v1.approval_decision_types_events.deleted',
} as const;
