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
import type { FsmState } from '../domain/fsm_state.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FsmStateKey {
    name: string;
}

export interface FsmStateWrite {
    id: string;
    machine_id: string;
    name: string;
    is_initial: boolean;
    is_terminal: boolean;
}

export interface FsmStateChange {
    write: FsmStateWrite;
    precondition: Precondition;
}

export interface FsmStateRemoval {
    key: FsmStateKey;
    precondition: Precondition;
}

export interface FsmStateLookup {
    key: FsmStateKey;
    fsm_state: FsmState | null;
}

export interface FsmStatesFilter {
    id_one_of: string[] | null;
}

export interface FsmStateEvent {
    event_id: string;
    key: FsmStateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FsmStateVersionKey {
    fsm_state: FsmStateKey;
    version: number;
}

export interface FsmStateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFsmStatesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FsmStatesFilter | null;
    as_of: string | null;
}

export interface ListFsmStatesResponse {
    result: Result;
    states: FsmState[];
    total: number;
}

export interface GetFsmStateRequest {
    key: FsmStateKey;
}

export interface GetFsmStateResponse {
    result: Result;
    fsm_state: FsmState | null;
}

export interface GetManyFsmStatesRequest {
    keys: FsmStateKey[];
}

export interface GetManyFsmStatesResponse {
    result: Result;
    entries: FsmStateLookup[];
}

export interface PutFsmStateRequest {
    change: FsmStateChange;
    intent: ChangeIntent;
}

export interface PutFsmStateResponse {
    result: Result;
    fsm_state: FsmState | null;
}

export interface PutManyFsmStatesRequest {
    changes: FsmStateChange[];
    intent: ChangeIntent;
}

export interface PutManyFsmStatesResponse {
    result: Result;
    states: FsmState[];
}

export interface DeleteFsmStateRequest {
    removal: FsmStateRemoval;
    intent: ChangeIntent;
}

export interface DeleteFsmStateResponse {
    result: Result;
}

export interface DeleteManyFsmStatesRequest {
    removals: FsmStateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFsmStatesResponse {
    result: Result;
}

export interface ListFsmStateVersionsRequest {
    key: FsmStateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FsmStateVersionsFilter | null;
}

export interface ListFsmStateVersionsResponse {
    result: Result;
    versions: FsmState[];
    total: number;
}

export interface GetFsmStateVersionRequest {
    key: FsmStateVersionKey;
}

export interface GetFsmStateVersionResponse {
    result: Result;
    version: FsmState | null;
}

export const subjects = {
    list_fsm_states_request: 'dq.v1.fsm_states.list',
    get_fsm_state_request: 'dq.v1.fsm_states.get',
    get_many_fsm_states_request: 'dq.v1.fsm_states.get_many',
    put_fsm_state_request: 'dq.v1.fsm_states.put',
    put_many_fsm_states_request: 'dq.v1.fsm_states.put_many',
    delete_fsm_state_request: 'dq.v1.fsm_states.delete',
    delete_many_fsm_states_request: 'dq.v1.fsm_states.delete_many',
    list_fsm_state_versions_request: 'dq.v1.fsm_states_versions.list',
    get_fsm_state_version_request: 'dq.v1.fsm_states_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fsm_states_request: true,
    get_fsm_state_request: true,
    get_many_fsm_states_request: true,
    put_fsm_state_request: true,
    put_many_fsm_states_request: true,
    delete_fsm_state_request: true,
    delete_many_fsm_states_request: true,
    list_fsm_state_versions_request: true,
    get_fsm_state_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.fsm_states_events.created',
    updated: 'dq.v1.fsm_states_events.updated',
    deleted: 'dq.v1.fsm_states_events.deleted',
} as const;
