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
import type { ScriptTwinInput } from '../domain/script_twin_input.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ScriptTwinInputKey {
    trade_type_code: string;
    sequence_number: number;
}

export interface ScriptTwinInputWrite {
    trade_type_code: string;
    sequence_number: number;
    script_name: string;
    input_name: string;
    data_type: string;
    is_array: boolean;
    source: string;
    transform: string | null;
}

export interface ScriptTwinInputChange {
    write: ScriptTwinInputWrite;
    precondition: Precondition;
}

export interface ScriptTwinInputRemoval {
    key: ScriptTwinInputKey;
    precondition: Precondition;
}

export interface ScriptTwinInputLookup {
    key: ScriptTwinInputKey;
    script_twin_input: ScriptTwinInput | null;
}

export interface ScriptTwinInputEvent {
    event_id: string;
    key: ScriptTwinInputKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptTwinInputVersionKey {
    script_twin_input: ScriptTwinInputKey;
    version: number;
}

export interface ScriptTwinInputVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptTwinInputsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListScriptTwinInputsResponse {
    result: Result;
    inputs: ScriptTwinInput[];
    total: number;
}

export interface GetScriptTwinInputRequest {
    key: ScriptTwinInputKey;
}

export interface GetScriptTwinInputResponse {
    result: Result;
    script_twin_input: ScriptTwinInput | null;
}

export interface GetManyScriptTwinInputsRequest {
    keys: ScriptTwinInputKey[];
}

export interface GetManyScriptTwinInputsResponse {
    result: Result;
    entries: ScriptTwinInputLookup[];
}

export interface PutScriptTwinInputRequest {
    change: ScriptTwinInputChange;
    intent: ChangeIntent;
}

export interface PutScriptTwinInputResponse {
    result: Result;
    script_twin_input: ScriptTwinInput | null;
}

export interface PutManyScriptTwinInputsRequest {
    changes: ScriptTwinInputChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptTwinInputsResponse {
    result: Result;
    inputs: ScriptTwinInput[];
}

export interface DeleteScriptTwinInputRequest {
    removal: ScriptTwinInputRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptTwinInputResponse {
    result: Result;
}

export interface DeleteManyScriptTwinInputsRequest {
    removals: ScriptTwinInputRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptTwinInputsResponse {
    result: Result;
}

export interface ListScriptTwinInputVersionsRequest {
    key: ScriptTwinInputKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptTwinInputVersionsFilter | null;
}

export interface ListScriptTwinInputVersionsResponse {
    result: Result;
    versions: ScriptTwinInput[];
    total: number;
}

export interface GetScriptTwinInputVersionRequest {
    key: ScriptTwinInputVersionKey;
}

export interface GetScriptTwinInputVersionResponse {
    result: Result;
    version: ScriptTwinInput | null;
}

export const subjects = {
    list_script_twin_inputs_request: 'trading.v1.script_twin_inputs.list',
    get_script_twin_input_request: 'trading.v1.script_twin_inputs.get',
    get_many_script_twin_inputs_request: 'trading.v1.script_twin_inputs.get_many',
    put_script_twin_input_request: 'trading.v1.script_twin_inputs.put',
    put_many_script_twin_inputs_request: 'trading.v1.script_twin_inputs.put_many',
    delete_script_twin_input_request: 'trading.v1.script_twin_inputs.delete',
    delete_many_script_twin_inputs_request: 'trading.v1.script_twin_inputs.delete_many',
    list_script_twin_input_versions_request: 'trading.v1.script_twin_inputs_versions.list',
    get_script_twin_input_version_request: 'trading.v1.script_twin_inputs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_script_twin_inputs_request: true,
    get_script_twin_input_request: true,
    get_many_script_twin_inputs_request: true,
    put_script_twin_input_request: true,
    put_many_script_twin_inputs_request: true,
    delete_script_twin_input_request: true,
    delete_many_script_twin_inputs_request: true,
    list_script_twin_input_versions_request: true,
    get_script_twin_input_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.script_twin_inputs.created',
    updated: 'trading.v1.events.script_twin_inputs.updated',
    deleted: 'trading.v1.events.script_twin_inputs.deleted',
} as const;
