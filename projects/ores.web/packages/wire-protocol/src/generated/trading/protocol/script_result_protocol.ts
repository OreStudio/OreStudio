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
import type { ScriptResult } from '../domain/script_result.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ScriptResultKey {
    script_name: string;
    purpose: string;
    sequence_number: number;
}

export interface ScriptResultWrite {
    script_name: string;
    purpose: string;
    sequence_number: number;
    variable: string;
    rename: string | null;
}

export interface ScriptResultChange {
    write: ScriptResultWrite;
    precondition: Precondition;
}

export interface ScriptResultRemoval {
    key: ScriptResultKey;
    precondition: Precondition;
}

export interface ScriptResultLookup {
    key: ScriptResultKey;
    script_result: ScriptResult | null;
}

export interface ScriptResultEvent {
    event_id: string;
    key: ScriptResultKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptResultVersionKey {
    script_result: ScriptResultKey;
    version: number;
}

export interface ScriptResultVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptResultsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListScriptResultsResponse {
    result: Result;
    results: ScriptResult[];
    total: number;
}

export interface GetScriptResultRequest {
    key: ScriptResultKey;
}

export interface GetScriptResultResponse {
    result: Result;
    script_result: ScriptResult | null;
}

export interface GetManyScriptResultsRequest {
    keys: ScriptResultKey[];
}

export interface GetManyScriptResultsResponse {
    result: Result;
    entries: ScriptResultLookup[];
}

export interface PutScriptResultRequest {
    change: ScriptResultChange;
    intent: ChangeIntent;
}

export interface PutScriptResultResponse {
    result: Result;
    script_result: ScriptResult | null;
}

export interface PutManyScriptResultsRequest {
    changes: ScriptResultChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptResultsResponse {
    result: Result;
    results: ScriptResult[];
}

export interface DeleteScriptResultRequest {
    removal: ScriptResultRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptResultResponse {
    result: Result;
}

export interface DeleteManyScriptResultsRequest {
    removals: ScriptResultRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptResultsResponse {
    result: Result;
}

export interface ListScriptResultVersionsRequest {
    key: ScriptResultKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptResultVersionsFilter | null;
}

export interface ListScriptResultVersionsResponse {
    result: Result;
    versions: ScriptResult[];
    total: number;
}

export interface GetScriptResultVersionRequest {
    key: ScriptResultVersionKey;
}

export interface GetScriptResultVersionResponse {
    result: Result;
    version: ScriptResult | null;
}

export const subjects = {
    list_script_results_request: 'trading.v1.script_results.list',
    get_script_result_request: 'trading.v1.script_results.get',
    get_many_script_results_request: 'trading.v1.script_results.get_many',
    put_script_result_request: 'trading.v1.script_results.put',
    put_many_script_results_request: 'trading.v1.script_results.put_many',
    delete_script_result_request: 'trading.v1.script_results.delete',
    delete_many_script_results_request: 'trading.v1.script_results.delete_many',
    list_script_result_versions_request: 'trading.v1.script_results_versions.list',
    get_script_result_version_request: 'trading.v1.script_results_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_script_results_request: true,
    get_script_result_request: true,
    get_many_script_results_request: true,
    put_script_result_request: true,
    put_many_script_results_request: true,
    delete_script_result_request: true,
    delete_many_script_results_request: true,
    list_script_result_versions_request: true,
    get_script_result_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.script_results.created',
    updated: 'trading.v1.events.script_results.updated',
    deleted: 'trading.v1.events.script_results.deleted',
} as const;
