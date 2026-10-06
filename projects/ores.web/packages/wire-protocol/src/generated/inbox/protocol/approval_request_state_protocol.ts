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
import type { ApprovalRequestState } from '../domain/approval_request_state.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ApprovalRequestStateKey {
    code: string;
}

export interface ApprovalRequestStateWrite {
    code: string;
    name: string;
    description: string;
    is_final: boolean;
    display_order: number;
}

export interface ApprovalRequestStateChange {
    write: ApprovalRequestStateWrite;
    precondition: Precondition;
}

export interface ApprovalRequestStateRemoval {
    key: ApprovalRequestStateKey;
    precondition: Precondition;
}

export interface ApprovalRequestStateLookup {
    key: ApprovalRequestStateKey;
    approval_request_state: ApprovalRequestState | null;
}

export interface ApprovalRequestStatesFilter {
    code_one_of: string[] | null;
}

export interface ApprovalRequestStateEvent {
    event_id: string;
    key: ApprovalRequestStateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ApprovalRequestStateVersionKey {
    approval_request_state: ApprovalRequestStateKey;
    version: number;
}

export interface ApprovalRequestStateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListApprovalRequestStatesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalRequestStatesFilter | null;
    as_of: string | null;
}

export interface ListApprovalRequestStatesResponse {
    result: Result;
    states: ApprovalRequestState[];
    total: number;
}

export interface GetApprovalRequestStateRequest {
    key: ApprovalRequestStateKey;
}

export interface GetApprovalRequestStateResponse {
    result: Result;
    approval_request_state: ApprovalRequestState | null;
}

export interface GetManyApprovalRequestStatesRequest {
    keys: ApprovalRequestStateKey[];
}

export interface GetManyApprovalRequestStatesResponse {
    result: Result;
    entries: ApprovalRequestStateLookup[];
}

export interface PutApprovalRequestStateRequest {
    change: ApprovalRequestStateChange;
    intent: ChangeIntent;
}

export interface PutApprovalRequestStateResponse {
    result: Result;
    approval_request_state: ApprovalRequestState | null;
}

export interface PutManyApprovalRequestStatesRequest {
    changes: ApprovalRequestStateChange[];
    intent: ChangeIntent;
}

export interface PutManyApprovalRequestStatesResponse {
    result: Result;
    states: ApprovalRequestState[];
}

export interface DeleteApprovalRequestStateRequest {
    removal: ApprovalRequestStateRemoval;
    intent: ChangeIntent;
}

export interface DeleteApprovalRequestStateResponse {
    result: Result;
}

export interface DeleteManyApprovalRequestStatesRequest {
    removals: ApprovalRequestStateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyApprovalRequestStatesResponse {
    result: Result;
}

export interface ListApprovalRequestStateVersionsRequest {
    key: ApprovalRequestStateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalRequestStateVersionsFilter | null;
}

export interface ListApprovalRequestStateVersionsResponse {
    result: Result;
    versions: ApprovalRequestState[];
    total: number;
}

export interface GetApprovalRequestStateVersionRequest {
    key: ApprovalRequestStateVersionKey;
}

export interface GetApprovalRequestStateVersionResponse {
    result: Result;
    version: ApprovalRequestState | null;
}

export const subjects = {
    list_approval_request_states_request: 'inbox.v1.approval_request_states.list',
    get_approval_request_state_request: 'inbox.v1.approval_request_states.get',
    get_many_approval_request_states_request: 'inbox.v1.approval_request_states.get_many',
    put_approval_request_state_request: 'inbox.v1.approval_request_states.put',
    put_many_approval_request_states_request: 'inbox.v1.approval_request_states.put_many',
    delete_approval_request_state_request: 'inbox.v1.approval_request_states.delete',
    delete_many_approval_request_states_request: 'inbox.v1.approval_request_states.delete_many',
    list_approval_request_state_versions_request: 'inbox.v1.approval_request_states_versions.list',
    get_approval_request_state_version_request: 'inbox.v1.approval_request_states_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_approval_request_states_request: true,
    get_approval_request_state_request: true,
    get_many_approval_request_states_request: true,
    put_approval_request_state_request: true,
    put_many_approval_request_states_request: true,
    delete_approval_request_state_request: true,
    delete_many_approval_request_states_request: true,
    list_approval_request_state_versions_request: true,
    get_approval_request_state_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.approval_request_states_events.created',
    updated: 'inbox.v1.approval_request_states_events.updated',
    deleted: 'inbox.v1.approval_request_states_events.deleted',
} as const;
