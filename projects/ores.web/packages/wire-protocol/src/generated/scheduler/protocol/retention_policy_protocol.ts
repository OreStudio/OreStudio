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
import type { RetentionPolicy } from '../domain/retention_policy.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RetentionPolicyKey {
    code: string;
}

export interface RetentionPolicyWrite {
    code: string;
    target_kind: string;
    target: string;
    action: string;
    retention_days: number;
    cadence: string;
    enabled: boolean;
    description: string;
}

export interface RetentionPolicyChange {
    write: RetentionPolicyWrite;
    precondition: Precondition;
}

export interface RetentionPolicyRemoval {
    key: RetentionPolicyKey;
    precondition: Precondition;
}

export interface RetentionPolicyLookup {
    key: RetentionPolicyKey;
    retention_policy: RetentionPolicy | null;
}

export interface RetentionPoliciesFilter {
    code_one_of: string[] | null;
}

export interface RetentionPolicyEvent {
    event_id: string;
    key: RetentionPolicyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RetentionPolicyVersionKey {
    retention_policy: RetentionPolicyKey;
    version: number;
}

export interface RetentionPolicyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRetentionPoliciesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RetentionPoliciesFilter | null;
    as_of: string | null;
}

export interface ListRetentionPoliciesResponse {
    result: Result;
    policies: RetentionPolicy[];
    total: number;
}

export interface GetRetentionPolicyRequest {
    key: RetentionPolicyKey;
}

export interface GetRetentionPolicyResponse {
    result: Result;
    retention_policy: RetentionPolicy | null;
}

export interface GetManyRetentionPoliciesRequest {
    keys: RetentionPolicyKey[];
}

export interface GetManyRetentionPoliciesResponse {
    result: Result;
    entries: RetentionPolicyLookup[];
}

export interface PutRetentionPolicyRequest {
    change: RetentionPolicyChange;
    intent: ChangeIntent;
}

export interface PutRetentionPolicyResponse {
    result: Result;
    retention_policy: RetentionPolicy | null;
}

export interface PutManyRetentionPoliciesRequest {
    changes: RetentionPolicyChange[];
    intent: ChangeIntent;
}

export interface PutManyRetentionPoliciesResponse {
    result: Result;
    policies: RetentionPolicy[];
}

export interface DeleteRetentionPolicyRequest {
    removal: RetentionPolicyRemoval;
    intent: ChangeIntent;
}

export interface DeleteRetentionPolicyResponse {
    result: Result;
}

export interface DeleteManyRetentionPoliciesRequest {
    removals: RetentionPolicyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRetentionPoliciesResponse {
    result: Result;
}

export interface ListRetentionPolicyVersionsRequest {
    key: RetentionPolicyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RetentionPolicyVersionsFilter | null;
}

export interface ListRetentionPolicyVersionsResponse {
    result: Result;
    versions: RetentionPolicy[];
    total: number;
}

export interface GetRetentionPolicyVersionRequest {
    key: RetentionPolicyVersionKey;
}

export interface GetRetentionPolicyVersionResponse {
    result: Result;
    version: RetentionPolicy | null;
}

export const subjects = {
    list_retention_policies_request: 'scheduler.v1.retention_policies.list',
    get_retention_policy_request: 'scheduler.v1.retention_policies.get',
    get_many_retention_policies_request: 'scheduler.v1.retention_policies.get_many',
    put_retention_policy_request: 'scheduler.v1.retention_policies.put',
    put_many_retention_policies_request: 'scheduler.v1.retention_policies.put_many',
    delete_retention_policy_request: 'scheduler.v1.retention_policies.delete',
    delete_many_retention_policies_request: 'scheduler.v1.retention_policies.delete_many',
    list_retention_policy_versions_request: 'scheduler.v1.retention_policies_versions.list',
    get_retention_policy_version_request: 'scheduler.v1.retention_policies_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_retention_policies_request: true,
    get_retention_policy_request: true,
    get_many_retention_policies_request: true,
    put_retention_policy_request: true,
    put_many_retention_policies_request: true,
    delete_retention_policy_request: true,
    delete_many_retention_policies_request: true,
    list_retention_policy_versions_request: true,
    get_retention_policy_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'scheduler.v1.events.retention_policies.created',
    updated: 'scheduler.v1.events.retention_policies.updated',
    deleted: 'scheduler.v1.events.retention_policies.deleted',
} as const;
