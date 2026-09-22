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
import type { ConcurrencyPolicy } from '../domain/concurrency_policy.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ConcurrencyPolicyKey {
    code: string;
}

export interface ConcurrencyPolicyWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface ConcurrencyPolicyChange {
    write: ConcurrencyPolicyWrite;
    precondition: Precondition;
}

export interface ConcurrencyPolicyRemoval {
    key: ConcurrencyPolicyKey;
    precondition: Precondition;
}

export interface ConcurrencyPolicyLookup {
    key: ConcurrencyPolicyKey;
    concurrency_policy: ConcurrencyPolicy | null;
}

export interface ConcurrencyPolicyEvent {
    event_id: string;
    key: ConcurrencyPolicyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConcurrencyPolicyVersionKey {
    concurrency_policy: ConcurrencyPolicyKey;
    version: number;
}

export interface ConcurrencyPolicyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConcurrencyPoliciesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListConcurrencyPoliciesResponse {
    result: Result;
    policies: ConcurrencyPolicy[];
    total: number;
}

export interface GetConcurrencyPolicyRequest {
    key: ConcurrencyPolicyKey;
}

export interface GetConcurrencyPolicyResponse {
    result: Result;
    concurrency_policy: ConcurrencyPolicy | null;
}

export interface GetManyConcurrencyPoliciesRequest {
    keys: ConcurrencyPolicyKey[];
}

export interface GetManyConcurrencyPoliciesResponse {
    result: Result;
    entries: ConcurrencyPolicyLookup[];
}

export interface PutConcurrencyPolicyRequest {
    change: ConcurrencyPolicyChange;
    intent: ChangeIntent;
}

export interface PutConcurrencyPolicyResponse {
    result: Result;
    concurrency_policy: ConcurrencyPolicy;
}

export interface PutManyConcurrencyPoliciesRequest {
    changes: ConcurrencyPolicyChange[];
    intent: ChangeIntent;
}

export interface PutManyConcurrencyPoliciesResponse {
    result: Result;
    policies: ConcurrencyPolicy[];
}

export interface DeleteConcurrencyPolicyRequest {
    removal: ConcurrencyPolicyRemoval;
    intent: ChangeIntent;
}

export interface DeleteConcurrencyPolicyResponse {
    result: Result;
}

export interface DeleteManyConcurrencyPoliciesRequest {
    removals: ConcurrencyPolicyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConcurrencyPoliciesResponse {
    result: Result;
}

export interface ListConcurrencyPolicyVersionsRequest {
    key: ConcurrencyPolicyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConcurrencyPolicyVersionsFilter | null;
}

export interface ListConcurrencyPolicyVersionsResponse {
    result: Result;
    versions: ConcurrencyPolicy[];
    total: number;
}

export interface GetConcurrencyPolicyVersionRequest {
    key: ConcurrencyPolicyVersionKey;
}

export interface GetConcurrencyPolicyVersionResponse {
    result: Result;
    version: ConcurrencyPolicy;
}

export const subjects = {
    list_concurrency_policies_request: "reporting.v1.concurrency_policies.list",
    get_concurrency_policy_request: "reporting.v1.concurrency_policies.get",
    get_many_concurrency_policies_request: "reporting.v1.concurrency_policies.get_many",
    put_concurrency_policy_request: "reporting.v1.concurrency_policies.put",
    put_many_concurrency_policies_request: "reporting.v1.concurrency_policies.put_many",
    delete_concurrency_policy_request: "reporting.v1.concurrency_policies.delete",
    delete_many_concurrency_policies_request: "reporting.v1.concurrency_policies.delete_many",
    list_concurrency_policy_versions_request: "reporting.v1.concurrency_policies_versions.list",
    get_concurrency_policy_version_request: "reporting.v1.concurrency_policies_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_concurrency_policies_request: true,
    get_concurrency_policy_request: true,
    get_many_concurrency_policies_request: true,
    put_concurrency_policy_request: true,
    put_many_concurrency_policies_request: true,
    delete_concurrency_policy_request: true,
    delete_many_concurrency_policies_request: true,
    list_concurrency_policy_versions_request: true,
    get_concurrency_policy_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "reporting.v1.concurrency_policies_events.created",
    updated: "reporting.v1.concurrency_policies_events.updated",
    deleted: "reporting.v1.concurrency_policies_events.deleted",
} as const;
