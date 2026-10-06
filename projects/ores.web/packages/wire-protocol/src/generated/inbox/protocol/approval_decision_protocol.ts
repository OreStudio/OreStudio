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
import type { ApprovalDecision } from '../domain/approval_decision.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ApprovalDecisionKey {
    id: string;
}

export interface ApprovalDecisionWrite {
    id: string;
    request_id: string;
    decision_code: string;
    decided_by: string;
    decided_at: string;
    comment: string;
}

export interface ApprovalDecisionChange {
    write: ApprovalDecisionWrite;
    precondition: Precondition;
}

export interface ApprovalDecisionRemoval {
    key: ApprovalDecisionKey;
    precondition: Precondition;
}

export interface ApprovalDecisionLookup {
    key: ApprovalDecisionKey;
    approval_decision: ApprovalDecision | null;
}

export interface ApprovalDecisionsFilter {
    request_id: string | null;
    id_one_of: string[] | null;
    request_id_one_of: string[] | null;
}

export interface ApprovalDecisionEvent {
    event_id: string;
    key: ApprovalDecisionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ApprovalDecisionVersionKey {
    approval_decision: ApprovalDecisionKey;
    version: number;
}

export interface ApprovalDecisionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListApprovalDecisionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalDecisionsFilter | null;
    as_of: string | null;
}

export interface ListApprovalDecisionsResponse {
    result: Result;
    decisions: ApprovalDecision[];
    total: number;
}

export interface GetApprovalDecisionRequest {
    key: ApprovalDecisionKey;
}

export interface GetApprovalDecisionResponse {
    result: Result;
    approval_decision: ApprovalDecision | null;
}

export interface GetManyApprovalDecisionsRequest {
    keys: ApprovalDecisionKey[];
}

export interface GetManyApprovalDecisionsResponse {
    result: Result;
    entries: ApprovalDecisionLookup[];
}

export interface PutApprovalDecisionRequest {
    change: ApprovalDecisionChange;
    intent: ChangeIntent;
}

export interface PutApprovalDecisionResponse {
    result: Result;
    approval_decision: ApprovalDecision | null;
}

export interface PutManyApprovalDecisionsRequest {
    changes: ApprovalDecisionChange[];
    intent: ChangeIntent;
}

export interface PutManyApprovalDecisionsResponse {
    result: Result;
    decisions: ApprovalDecision[];
}

export interface DeleteApprovalDecisionRequest {
    removal: ApprovalDecisionRemoval;
    intent: ChangeIntent;
}

export interface DeleteApprovalDecisionResponse {
    result: Result;
}

export interface DeleteManyApprovalDecisionsRequest {
    removals: ApprovalDecisionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyApprovalDecisionsResponse {
    result: Result;
}

export interface ListByRequestIdApprovalDecisionsRequest {
    request_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalDecisionsFilter | null;
}

export interface ListByRequestIdApprovalDecisionsResponse {
    result: Result;
    decisions: ApprovalDecision[];
    total: number;
}

export interface ListApprovalDecisionVersionsRequest {
    key: ApprovalDecisionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalDecisionVersionsFilter | null;
}

export interface ListApprovalDecisionVersionsResponse {
    result: Result;
    versions: ApprovalDecision[];
    total: number;
}

export interface GetApprovalDecisionVersionRequest {
    key: ApprovalDecisionVersionKey;
}

export interface GetApprovalDecisionVersionResponse {
    result: Result;
    version: ApprovalDecision | null;
}

export const subjects = {
    list_approval_decisions_request: 'inbox.v1.approval_decisions.list',
    get_approval_decision_request: 'inbox.v1.approval_decisions.get',
    get_many_approval_decisions_request: 'inbox.v1.approval_decisions.get_many',
    put_approval_decision_request: 'inbox.v1.approval_decisions.put',
    put_many_approval_decisions_request: 'inbox.v1.approval_decisions.put_many',
    delete_approval_decision_request: 'inbox.v1.approval_decisions.delete',
    delete_many_approval_decisions_request: 'inbox.v1.approval_decisions.delete_many',
    list_by_request_id_approval_decisions_request: 'inbox.v1.approval_decisions.list_by_request_id',
    list_approval_decision_versions_request: 'inbox.v1.approval_decisions_versions.list',
    get_approval_decision_version_request: 'inbox.v1.approval_decisions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_approval_decisions_request: true,
    get_approval_decision_request: true,
    get_many_approval_decisions_request: true,
    put_approval_decision_request: true,
    put_many_approval_decisions_request: true,
    delete_approval_decision_request: true,
    delete_many_approval_decisions_request: true,
    list_by_request_id_approval_decisions_request: true,
    list_approval_decision_versions_request: true,
    get_approval_decision_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.approval_decisions_events.created',
    updated: 'inbox.v1.approval_decisions_events.updated',
    deleted: 'inbox.v1.approval_decisions_events.deleted',
} as const;
