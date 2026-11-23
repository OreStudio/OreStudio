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
import type { ScriptInputCode } from '../domain/script_input_code.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ScriptInputCodeKey {
    script_name: string;
    sequence_number: number;
}

export interface ScriptInputCodeWrite {
    script_name: string;
    sequence_number: number;
    input_name: string;
    code: number;
    label: string;
    script_version: number;
}

export interface ScriptInputCodeChange {
    write: ScriptInputCodeWrite;
    precondition: Precondition;
}

export interface ScriptInputCodeRemoval {
    key: ScriptInputCodeKey;
    precondition: Precondition;
}

export interface ScriptInputCodeLookup {
    key: ScriptInputCodeKey;
    script_input_code: ScriptInputCode | null;
}

export interface ScriptInputCodeEvent {
    event_id: string;
    key: ScriptInputCodeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptInputCodeVersionKey {
    script_input_code: ScriptInputCodeKey;
    version: number;
}

export interface ScriptInputCodeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptInputCodesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListScriptInputCodesResponse {
    result: Result;
    codes: ScriptInputCode[];
    total: number;
}

export interface GetScriptInputCodeRequest {
    key: ScriptInputCodeKey;
}

export interface GetScriptInputCodeResponse {
    result: Result;
    script_input_code: ScriptInputCode | null;
}

export interface GetManyScriptInputCodesRequest {
    keys: ScriptInputCodeKey[];
}

export interface GetManyScriptInputCodesResponse {
    result: Result;
    entries: ScriptInputCodeLookup[];
}

export interface PutScriptInputCodeRequest {
    change: ScriptInputCodeChange;
    intent: ChangeIntent;
}

export interface PutScriptInputCodeResponse {
    result: Result;
    script_input_code: ScriptInputCode | null;
}

export interface PutManyScriptInputCodesRequest {
    changes: ScriptInputCodeChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptInputCodesResponse {
    result: Result;
    codes: ScriptInputCode[];
}

export interface DeleteScriptInputCodeRequest {
    removal: ScriptInputCodeRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptInputCodeResponse {
    result: Result;
}

export interface DeleteManyScriptInputCodesRequest {
    removals: ScriptInputCodeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptInputCodesResponse {
    result: Result;
}

export interface ListScriptInputCodeVersionsRequest {
    key: ScriptInputCodeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptInputCodeVersionsFilter | null;
}

export interface ListScriptInputCodeVersionsResponse {
    result: Result;
    versions: ScriptInputCode[];
    total: number;
}

export interface GetScriptInputCodeVersionRequest {
    key: ScriptInputCodeVersionKey;
}

export interface GetScriptInputCodeVersionResponse {
    result: Result;
    version: ScriptInputCode | null;
}

export const subjects = {
    list_script_input_codes_request: 'trading.v1.script_input_codes.list',
    get_script_input_code_request: 'trading.v1.script_input_codes.get',
    get_many_script_input_codes_request: 'trading.v1.script_input_codes.get_many',
    put_script_input_code_request: 'trading.v1.script_input_codes.put',
    put_many_script_input_codes_request: 'trading.v1.script_input_codes.put_many',
    delete_script_input_code_request: 'trading.v1.script_input_codes.delete',
    delete_many_script_input_codes_request: 'trading.v1.script_input_codes.delete_many',
    list_script_input_code_versions_request: 'trading.v1.script_input_codes_versions.list',
    get_script_input_code_version_request: 'trading.v1.script_input_codes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_script_input_codes_request: true,
    get_script_input_code_request: true,
    get_many_script_input_codes_request: true,
    put_script_input_code_request: true,
    put_many_script_input_codes_request: true,
    delete_script_input_code_request: true,
    delete_many_script_input_codes_request: true,
    list_script_input_code_versions_request: true,
    get_script_input_code_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.script_input_codes.created',
    updated: 'trading.v1.events.script_input_codes.updated',
    deleted: 'trading.v1.events.script_input_codes.deleted',
} as const;
