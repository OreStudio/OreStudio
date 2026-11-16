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
import type { Script } from '../domain/script.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ScriptKey {
    name: string;
}

export interface ScriptWrite {
    name: string;
    product_tag: string | null;
    description: string | null;
}

export interface ScriptChange {
    write: ScriptWrite;
    precondition: Precondition;
}

export interface ScriptRemoval {
    key: ScriptKey;
    precondition: Precondition;
}

export interface ScriptLookup {
    key: ScriptKey;
    script: Script | null;
}

export interface ScriptsFilter {
    name_one_of: string[] | null;
}

export interface ScriptEvent {
    event_id: string;
    key: ScriptKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptVersionKey {
    script: ScriptKey;
    version: number;
}

export interface ScriptVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptsFilter | null;
    as_of: string | null;
}

export interface ListScriptsResponse {
    result: Result;
    scripts: Script[];
    total: number;
}

export interface GetScriptRequest {
    key: ScriptKey;
}

export interface GetScriptResponse {
    result: Result;
    script: Script | null;
}

export interface GetManyScriptsRequest {
    keys: ScriptKey[];
}

export interface GetManyScriptsResponse {
    result: Result;
    entries: ScriptLookup[];
}

export interface PutScriptRequest {
    change: ScriptChange;
    intent: ChangeIntent;
}

export interface PutScriptResponse {
    result: Result;
    script: Script | null;
}

export interface PutManyScriptsRequest {
    changes: ScriptChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptsResponse {
    result: Result;
    scripts: Script[];
}

export interface DeleteScriptRequest {
    removal: ScriptRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptResponse {
    result: Result;
}

export interface DeleteManyScriptsRequest {
    removals: ScriptRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptsResponse {
    result: Result;
}

export interface ListScriptVersionsRequest {
    key: ScriptKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptVersionsFilter | null;
}

export interface ListScriptVersionsResponse {
    result: Result;
    versions: Script[];
    total: number;
}

export interface GetScriptVersionRequest {
    key: ScriptVersionKey;
}

export interface GetScriptVersionResponse {
    result: Result;
    version: Script | null;
}

export const subjects = {
    list_scripts_request: 'trading.v1.scripts.list',
    get_script_request: 'trading.v1.scripts.get',
    get_many_scripts_request: 'trading.v1.scripts.get_many',
    put_script_request: 'trading.v1.scripts.put',
    put_many_scripts_request: 'trading.v1.scripts.put_many',
    delete_script_request: 'trading.v1.scripts.delete',
    delete_many_scripts_request: 'trading.v1.scripts.delete_many',
    list_script_versions_request: 'trading.v1.scripts_versions.list',
    get_script_version_request: 'trading.v1.scripts_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_scripts_request: true,
    get_script_request: true,
    get_many_scripts_request: true,
    put_script_request: true,
    put_many_scripts_request: true,
    delete_script_request: true,
    delete_many_scripts_request: true,
    list_script_versions_request: true,
    get_script_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.scripts.created',
    updated: 'trading.v1.events.scripts.updated',
    deleted: 'trading.v1.events.scripts.deleted',
} as const;
