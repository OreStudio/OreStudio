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
import type { ScriptCoarsening } from '../domain/script_coarsening.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ScriptCoarseningKey {
    script_name: string;
    purpose: string;
    sequence_number: number;
}

export interface ScriptCoarseningWrite {
    script_name: string;
    purpose: string;
    sequence_number: number;
    eligible_schedule: string;
}

export interface ScriptCoarseningChange {
    write: ScriptCoarseningWrite;
    precondition: Precondition;
}

export interface ScriptCoarseningRemoval {
    key: ScriptCoarseningKey;
    precondition: Precondition;
}

export interface ScriptCoarseningLookup {
    key: ScriptCoarseningKey;
    script_coarsening: ScriptCoarsening | null;
}

export interface ScriptCoarseningEvent {
    event_id: string;
    key: ScriptCoarseningKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptCoarseningVersionKey {
    script_coarsening: ScriptCoarseningKey;
    version: number;
}

export interface ScriptCoarseningVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptCoarseningsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListScriptCoarseningsResponse {
    result: Result;
    coarsenings: ScriptCoarsening[];
    total: number;
}

export interface GetScriptCoarseningRequest {
    key: ScriptCoarseningKey;
}

export interface GetScriptCoarseningResponse {
    result: Result;
    script_coarsening: ScriptCoarsening | null;
}

export interface GetManyScriptCoarseningsRequest {
    keys: ScriptCoarseningKey[];
}

export interface GetManyScriptCoarseningsResponse {
    result: Result;
    entries: ScriptCoarseningLookup[];
}

export interface PutScriptCoarseningRequest {
    change: ScriptCoarseningChange;
    intent: ChangeIntent;
}

export interface PutScriptCoarseningResponse {
    result: Result;
    script_coarsening: ScriptCoarsening | null;
}

export interface PutManyScriptCoarseningsRequest {
    changes: ScriptCoarseningChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptCoarseningsResponse {
    result: Result;
    coarsenings: ScriptCoarsening[];
}

export interface DeleteScriptCoarseningRequest {
    removal: ScriptCoarseningRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptCoarseningResponse {
    result: Result;
}

export interface DeleteManyScriptCoarseningsRequest {
    removals: ScriptCoarseningRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptCoarseningsResponse {
    result: Result;
}

export interface ListScriptCoarseningVersionsRequest {
    key: ScriptCoarseningKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptCoarseningVersionsFilter | null;
}

export interface ListScriptCoarseningVersionsResponse {
    result: Result;
    versions: ScriptCoarsening[];
    total: number;
}

export interface GetScriptCoarseningVersionRequest {
    key: ScriptCoarseningVersionKey;
}

export interface GetScriptCoarseningVersionResponse {
    result: Result;
    version: ScriptCoarsening | null;
}

export const subjects = {
    list_script_coarsenings_request: 'trading.v1.script_coarsenings.list',
    get_script_coarsening_request: 'trading.v1.script_coarsenings.get',
    get_many_script_coarsenings_request: 'trading.v1.script_coarsenings.get_many',
    put_script_coarsening_request: 'trading.v1.script_coarsenings.put',
    put_many_script_coarsenings_request: 'trading.v1.script_coarsenings.put_many',
    delete_script_coarsening_request: 'trading.v1.script_coarsenings.delete',
    delete_many_script_coarsenings_request: 'trading.v1.script_coarsenings.delete_many',
    list_script_coarsening_versions_request: 'trading.v1.script_coarsenings_versions.list',
    get_script_coarsening_version_request: 'trading.v1.script_coarsenings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_script_coarsenings_request: true,
    get_script_coarsening_request: true,
    get_many_script_coarsenings_request: true,
    put_script_coarsening_request: true,
    put_many_script_coarsenings_request: true,
    delete_script_coarsening_request: true,
    delete_many_script_coarsenings_request: true,
    list_script_coarsening_versions_request: true,
    get_script_coarsening_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.script_coarsenings.created',
    updated: 'trading.v1.events.script_coarsenings.updated',
    deleted: 'trading.v1.events.script_coarsenings.deleted',
} as const;
