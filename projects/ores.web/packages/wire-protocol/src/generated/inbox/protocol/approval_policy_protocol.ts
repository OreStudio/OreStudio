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
import type { ApprovalPolicy } from '../domain/approval_policy.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ApprovalPolicyKey {
    code: string;
}

export interface ApprovalPolicyWrite {
    code: string;
    name: string;
    description: string;
    entity_type: string;
    operation: string;
    field_name: string | null;
    part_code: string;
    display_order: number;
}

export interface ApprovalPolicyChange {
    write: ApprovalPolicyWrite;
    precondition: Precondition;
}

export interface ApprovalPolicyRemoval {
    key: ApprovalPolicyKey;
    precondition: Precondition;
}

export interface ApprovalPolicyLookup {
    key: ApprovalPolicyKey;
    approval_policy: ApprovalPolicy | null;
}

export interface ApprovalPoliciesFilter {
    code_one_of: string[] | null;
}

export interface ApprovalPolicyEvent {
    event_id: string;
    key: ApprovalPolicyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ApprovalPolicyVersionKey {
    approval_policy: ApprovalPolicyKey;
    version: number;
}

export interface ApprovalPolicyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListApprovalPoliciesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalPoliciesFilter | null;
    as_of: string | null;
}

export interface ListApprovalPoliciesResponse {
    result: Result;
    policies: ApprovalPolicy[];
    total: number;
}

export interface GetApprovalPolicyRequest {
    key: ApprovalPolicyKey;
}

export interface GetApprovalPolicyResponse {
    result: Result;
    approval_policy: ApprovalPolicy | null;
}

export interface GetManyApprovalPoliciesRequest {
    keys: ApprovalPolicyKey[];
}

export interface GetManyApprovalPoliciesResponse {
    result: Result;
    entries: ApprovalPolicyLookup[];
}

export interface PutApprovalPolicyRequest {
    change: ApprovalPolicyChange;
    intent: ChangeIntent;
}

export interface PutApprovalPolicyResponse {
    result: Result;
    approval_policy: ApprovalPolicy | null;
}

export interface PutManyApprovalPoliciesRequest {
    changes: ApprovalPolicyChange[];
    intent: ChangeIntent;
}

export interface PutManyApprovalPoliciesResponse {
    result: Result;
    policies: ApprovalPolicy[];
}

export interface DeleteApprovalPolicyRequest {
    removal: ApprovalPolicyRemoval;
    intent: ChangeIntent;
}

export interface DeleteApprovalPolicyResponse {
    result: Result;
}

export interface DeleteManyApprovalPoliciesRequest {
    removals: ApprovalPolicyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyApprovalPoliciesResponse {
    result: Result;
}

export interface ListApprovalPolicyVersionsRequest {
    key: ApprovalPolicyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalPolicyVersionsFilter | null;
}

export interface ListApprovalPolicyVersionsResponse {
    result: Result;
    versions: ApprovalPolicy[];
    total: number;
}

export interface GetApprovalPolicyVersionRequest {
    key: ApprovalPolicyVersionKey;
}

export interface GetApprovalPolicyVersionResponse {
    result: Result;
    version: ApprovalPolicy | null;
}

export const subjects = {
    list_approval_policies_request: 'inbox.v1.approval_policies.list',
    get_approval_policy_request: 'inbox.v1.approval_policies.get',
    get_many_approval_policies_request: 'inbox.v1.approval_policies.get_many',
    put_approval_policy_request: 'inbox.v1.approval_policies.put',
    put_many_approval_policies_request: 'inbox.v1.approval_policies.put_many',
    delete_approval_policy_request: 'inbox.v1.approval_policies.delete',
    delete_many_approval_policies_request: 'inbox.v1.approval_policies.delete_many',
    list_approval_policy_versions_request: 'inbox.v1.approval_policies_versions.list',
    get_approval_policy_version_request: 'inbox.v1.approval_policies_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_approval_policies_request: true,
    get_approval_policy_request: true,
    get_many_approval_policies_request: true,
    put_approval_policy_request: true,
    put_many_approval_policies_request: true,
    delete_approval_policy_request: true,
    delete_many_approval_policies_request: true,
    list_approval_policy_versions_request: true,
    get_approval_policy_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.approval_policies_events.created',
    updated: 'inbox.v1.approval_policies_events.updated',
    deleted: 'inbox.v1.approval_policies_events.deleted',
} as const;
