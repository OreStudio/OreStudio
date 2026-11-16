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
import type { ScriptInput } from '../domain/script_input.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ScriptInputKey {
    script_name: string;
    sequence_number: number;
}

export interface ScriptInputWrite {
    script_name: string;
    sequence_number: number;
    input_name: string;
    data_type: string;
    is_array: boolean;
    value_rule: string | null;
    description: string | null;
}

export interface ScriptInputChange {
    write: ScriptInputWrite;
    precondition: Precondition;
}

export interface ScriptInputRemoval {
    key: ScriptInputKey;
    precondition: Precondition;
}

export interface ScriptInputLookup {
    key: ScriptInputKey;
    script_input: ScriptInput | null;
}

export interface ScriptInputEvent {
    event_id: string;
    key: ScriptInputKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptInputVersionKey {
    script_input: ScriptInputKey;
    version: number;
}

export interface ScriptInputVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptInputsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListScriptInputsResponse {
    result: Result;
    inputs: ScriptInput[];
    total: number;
}

export interface GetScriptInputRequest {
    key: ScriptInputKey;
}

export interface GetScriptInputResponse {
    result: Result;
    script_input: ScriptInput | null;
}

export interface GetManyScriptInputsRequest {
    keys: ScriptInputKey[];
}

export interface GetManyScriptInputsResponse {
    result: Result;
    entries: ScriptInputLookup[];
}

export interface PutScriptInputRequest {
    change: ScriptInputChange;
    intent: ChangeIntent;
}

export interface PutScriptInputResponse {
    result: Result;
    script_input: ScriptInput | null;
}

export interface PutManyScriptInputsRequest {
    changes: ScriptInputChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptInputsResponse {
    result: Result;
    inputs: ScriptInput[];
}

export interface DeleteScriptInputRequest {
    removal: ScriptInputRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptInputResponse {
    result: Result;
}

export interface DeleteManyScriptInputsRequest {
    removals: ScriptInputRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptInputsResponse {
    result: Result;
}

export interface ListScriptInputVersionsRequest {
    key: ScriptInputKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptInputVersionsFilter | null;
}

export interface ListScriptInputVersionsResponse {
    result: Result;
    versions: ScriptInput[];
    total: number;
}

export interface GetScriptInputVersionRequest {
    key: ScriptInputVersionKey;
}

export interface GetScriptInputVersionResponse {
    result: Result;
    version: ScriptInput | null;
}

export const subjects = {
    list_script_inputs_request: 'trading.v1.script_inputs.list',
    get_script_input_request: 'trading.v1.script_inputs.get',
    get_many_script_inputs_request: 'trading.v1.script_inputs.get_many',
    put_script_input_request: 'trading.v1.script_inputs.put',
    put_many_script_inputs_request: 'trading.v1.script_inputs.put_many',
    delete_script_input_request: 'trading.v1.script_inputs.delete',
    delete_many_script_inputs_request: 'trading.v1.script_inputs.delete_many',
    list_script_input_versions_request: 'trading.v1.script_inputs_versions.list',
    get_script_input_version_request: 'trading.v1.script_inputs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_script_inputs_request: true,
    get_script_input_request: true,
    get_many_script_inputs_request: true,
    put_script_input_request: true,
    put_many_script_inputs_request: true,
    delete_script_input_request: true,
    delete_many_script_inputs_request: true,
    list_script_input_versions_request: true,
    get_script_input_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.script_inputs.created',
    updated: 'trading.v1.events.script_inputs.updated',
    deleted: 'trading.v1.events.script_inputs.deleted',
} as const;
