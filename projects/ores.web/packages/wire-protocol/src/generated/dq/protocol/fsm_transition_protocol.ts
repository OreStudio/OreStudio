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
import type { FsmTransition } from '../domain/fsm_transition.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FsmTransitionKey {
    name: string;
}

export interface FsmTransitionWrite {
    id: string;
    machine_id: string;
    from_state_id: string | null;
    to_state_id: string;
    name: string;
    guard_function: string | null;
}

export interface FsmTransitionChange {
    write: FsmTransitionWrite;
    precondition: Precondition;
}

export interface FsmTransitionRemoval {
    key: FsmTransitionKey;
    precondition: Precondition;
}

export interface FsmTransitionLookup {
    key: FsmTransitionKey;
    fsm_transition: FsmTransition | null;
}

export interface FsmTransitionEvent {
    event_id: string;
    key: FsmTransitionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FsmTransitionVersionKey {
    fsm_transition: FsmTransitionKey;
    version: number;
}

export interface FsmTransitionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFsmTransitionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListFsmTransitionsResponse {
    result: Result;
    transitions: FsmTransition[];
    total: number;
}

export interface GetFsmTransitionRequest {
    key: FsmTransitionKey;
}

export interface GetFsmTransitionResponse {
    result: Result;
    fsm_transition: FsmTransition | null;
}

export interface GetManyFsmTransitionsRequest {
    keys: FsmTransitionKey[];
}

export interface GetManyFsmTransitionsResponse {
    result: Result;
    entries: FsmTransitionLookup[];
}

export interface PutFsmTransitionRequest {
    change: FsmTransitionChange;
    intent: ChangeIntent;
}

export interface PutFsmTransitionResponse {
    result: Result;
    fsm_transition: FsmTransition;
}

export interface PutManyFsmTransitionsRequest {
    changes: FsmTransitionChange[];
    intent: ChangeIntent;
}

export interface PutManyFsmTransitionsResponse {
    result: Result;
    transitions: FsmTransition[];
}

export interface DeleteFsmTransitionRequest {
    removal: FsmTransitionRemoval;
    intent: ChangeIntent;
}

export interface DeleteFsmTransitionResponse {
    result: Result;
}

export interface DeleteManyFsmTransitionsRequest {
    removals: FsmTransitionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFsmTransitionsResponse {
    result: Result;
}

export interface ListFsmTransitionVersionsRequest {
    key: FsmTransitionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FsmTransitionVersionsFilter | null;
}

export interface ListFsmTransitionVersionsResponse {
    result: Result;
    versions: FsmTransition[];
    total: number;
}

export interface GetFsmTransitionVersionRequest {
    key: FsmTransitionVersionKey;
}

export interface GetFsmTransitionVersionResponse {
    result: Result;
    version: FsmTransition;
}

export const subjects = {
    list_fsm_transitions_request: 'dq.v1.fsm_transitions.list',
    get_fsm_transition_request: 'dq.v1.fsm_transitions.get',
    get_many_fsm_transitions_request: 'dq.v1.fsm_transitions.get_many',
    put_fsm_transition_request: 'dq.v1.fsm_transitions.put',
    put_many_fsm_transitions_request: 'dq.v1.fsm_transitions.put_many',
    delete_fsm_transition_request: 'dq.v1.fsm_transitions.delete',
    delete_many_fsm_transitions_request: 'dq.v1.fsm_transitions.delete_many',
    list_fsm_transition_versions_request: 'dq.v1.fsm_transitions_versions.list',
    get_fsm_transition_version_request: 'dq.v1.fsm_transitions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fsm_transitions_request: true,
    get_fsm_transition_request: true,
    get_many_fsm_transitions_request: true,
    put_fsm_transition_request: true,
    put_many_fsm_transitions_request: true,
    delete_fsm_transition_request: true,
    delete_many_fsm_transitions_request: true,
    list_fsm_transition_versions_request: true,
    get_fsm_transition_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.fsm_transitions_events.created',
    updated: 'dq.v1.fsm_transitions_events.updated',
    deleted: 'dq.v1.fsm_transitions_events.deleted',
} as const;
