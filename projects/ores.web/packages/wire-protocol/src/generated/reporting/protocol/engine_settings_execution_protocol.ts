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
import type { EngineSettingsExecution } from '../domain/engine_settings_execution.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface EngineSettingsExecutionKey {
    id: string;
}

export interface EngineSettingsExecutionWrite {
    id: string;
    engine_settings_id: string;
    n_threads: number | null;
    log_mask: number | null;
    gzip_compression_level: number | null;
}

export interface EngineSettingsExecutionChange {
    write: EngineSettingsExecutionWrite;
    precondition: Precondition;
}

export interface EngineSettingsExecutionRemoval {
    key: EngineSettingsExecutionKey;
    precondition: Precondition;
}

export interface EngineSettingsExecutionLookup {
    key: EngineSettingsExecutionKey;
    engine_settings_execution: EngineSettingsExecution | null;
}

export interface EngineSettingsExecutionFilter {
    engine_settings_id: string | null;
    id_one_of: string[] | null;
    engine_settings_id_one_of: string[] | null;
}

export interface EngineSettingsExecutionEvent {
    event_id: string;
    key: EngineSettingsExecutionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EngineSettingsExecutionVersionKey {
    engine_settings_execution: EngineSettingsExecutionKey;
    version: number;
}

export interface EngineSettingsExecutionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEngineSettingsExecutionRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsExecutionFilter | null;
    as_of: string | null;
}

export interface ListEngineSettingsExecutionResponse {
    result: Result;
    engine_settings_execution: EngineSettingsExecution[];
    total: number;
}

export interface GetEngineSettingsExecutionRequest {
    key: EngineSettingsExecutionKey;
}

export interface GetEngineSettingsExecutionResponse {
    result: Result;
    engine_settings_execution: EngineSettingsExecution | null;
}

export interface GetManyEngineSettingsExecutionRequest {
    keys: EngineSettingsExecutionKey[];
}

export interface GetManyEngineSettingsExecutionResponse {
    result: Result;
    entries: EngineSettingsExecutionLookup[];
}

export interface PutEngineSettingsExecutionRequest {
    change: EngineSettingsExecutionChange;
    intent: ChangeIntent;
}

export interface PutEngineSettingsExecutionResponse {
    result: Result;
    engine_settings_execution: EngineSettingsExecution | null;
}

export interface PutManyEngineSettingsExecutionRequest {
    changes: EngineSettingsExecutionChange[];
    intent: ChangeIntent;
}

export interface PutManyEngineSettingsExecutionResponse {
    result: Result;
    engine_settings_execution: EngineSettingsExecution[];
}

export interface DeleteEngineSettingsExecutionRequest {
    removal: EngineSettingsExecutionRemoval;
    intent: ChangeIntent;
}

export interface DeleteEngineSettingsExecutionResponse {
    result: Result;
}

export interface DeleteManyEngineSettingsExecutionRequest {
    removals: EngineSettingsExecutionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEngineSettingsExecutionResponse {
    result: Result;
}

export interface ListByEngineSettingsIdEngineSettingsExecutionRequest {
    engine_settings_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsExecutionFilter | null;
}

export interface ListByEngineSettingsIdEngineSettingsExecutionResponse {
    result: Result;
    engine_settings_execution: EngineSettingsExecution[];
    total: number;
}

export interface ListEngineSettingsExecutionVersionsRequest {
    key: EngineSettingsExecutionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsExecutionVersionsFilter | null;
}

export interface ListEngineSettingsExecutionVersionsResponse {
    result: Result;
    versions: EngineSettingsExecution[];
    total: number;
}

export interface GetEngineSettingsExecutionVersionRequest {
    key: EngineSettingsExecutionVersionKey;
}

export interface GetEngineSettingsExecutionVersionResponse {
    result: Result;
    version: EngineSettingsExecution | null;
}

export const subjects = {
    list_engine_settings_execution_request: 'reporting.v1.engine_settings_execution.list',
    get_engine_settings_execution_request: 'reporting.v1.engine_settings_execution.get',
    get_many_engine_settings_execution_request: 'reporting.v1.engine_settings_execution.get_many',
    put_engine_settings_execution_request: 'reporting.v1.engine_settings_execution.put',
    put_many_engine_settings_execution_request: 'reporting.v1.engine_settings_execution.put_many',
    delete_engine_settings_execution_request: 'reporting.v1.engine_settings_execution.delete',
    delete_many_engine_settings_execution_request:
        'reporting.v1.engine_settings_execution.delete_many',
    list_by_engine_settings_id_engine_settings_execution_request:
        'reporting.v1.engine_settings_execution.list_by_engine_settings_id',
    list_engine_settings_execution_versions_request:
        'reporting.v1.engine_settings_execution_versions.list',
    get_engine_settings_execution_version_request:
        'reporting.v1.engine_settings_execution_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_engine_settings_execution_request: true,
    get_engine_settings_execution_request: true,
    get_many_engine_settings_execution_request: true,
    put_engine_settings_execution_request: true,
    put_many_engine_settings_execution_request: true,
    delete_engine_settings_execution_request: true,
    delete_many_engine_settings_execution_request: true,
    list_by_engine_settings_id_engine_settings_execution_request: true,
    list_engine_settings_execution_versions_request: true,
    get_engine_settings_execution_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.engine_settings_execution_events.created',
    updated: 'reporting.v1.engine_settings_execution_events.updated',
    deleted: 'reporting.v1.engine_settings_execution_events.deleted',
} as const;
