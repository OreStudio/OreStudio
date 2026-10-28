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
import type { EngineSettingsChange } from '../domain/engine_settings_change.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface EngineSettingsChangeKey {
    id: string;
}

export interface EngineSettingsChangeLookup {
    key: EngineSettingsChangeKey;
    engine_settings_change: EngineSettingsChange | null;
}

export interface EngineSettingsChangesFilter {
    request_id: string | null;
    engine_settings_id: string | null;
    id_one_of: string[] | null;
    request_id_one_of: string[] | null;
    engine_settings_id_one_of: string[] | null;
}

export interface EngineSettingsChangeEvent {
    event_id: string;
    key: EngineSettingsChangeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EngineSettingsChangeVersionKey {
    engine_settings_change: EngineSettingsChangeKey;
    version: number;
}

export interface EngineSettingsChangeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEngineSettingsChangesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsChangesFilter | null;
    as_of: string | null;
}

export interface ListEngineSettingsChangesResponse {
    result: Result;
    engine_settings_changes: EngineSettingsChange[];
    total: number;
}

export interface GetEngineSettingsChangeRequest {
    key: EngineSettingsChangeKey;
}

export interface GetEngineSettingsChangeResponse {
    result: Result;
    engine_settings_change: EngineSettingsChange | null;
}

export interface GetManyEngineSettingsChangesRequest {
    keys: EngineSettingsChangeKey[];
}

export interface GetManyEngineSettingsChangesResponse {
    result: Result;
    entries: EngineSettingsChangeLookup[];
}

export interface ListByRequestIdEngineSettingsChangesRequest {
    request_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsChangesFilter | null;
}

export interface ListByRequestIdEngineSettingsChangesResponse {
    result: Result;
    engine_settings_changes: EngineSettingsChange[];
    total: number;
}

export interface ListByEngineSettingsIdEngineSettingsChangesRequest {
    engine_settings_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsChangesFilter | null;
}

export interface ListByEngineSettingsIdEngineSettingsChangesResponse {
    result: Result;
    engine_settings_changes: EngineSettingsChange[];
    total: number;
}

export interface ListEngineSettingsChangeVersionsRequest {
    key: EngineSettingsChangeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsChangeVersionsFilter | null;
}

export interface ListEngineSettingsChangeVersionsResponse {
    result: Result;
    versions: EngineSettingsChange[];
    total: number;
}

export interface GetEngineSettingsChangeVersionRequest {
    key: EngineSettingsChangeVersionKey;
}

export interface GetEngineSettingsChangeVersionResponse {
    result: Result;
    version: EngineSettingsChange | null;
}

export const subjects = {
    list_engine_settings_changes_request: 'reporting.v1.engine_settings_changes.list',
    get_engine_settings_change_request: 'reporting.v1.engine_settings_changes.get',
    get_many_engine_settings_changes_request: 'reporting.v1.engine_settings_changes.get_many',
    list_by_request_id_engine_settings_changes_request:
        'reporting.v1.engine_settings_changes.list_by_request_id',
    list_by_engine_settings_id_engine_settings_changes_request:
        'reporting.v1.engine_settings_changes.list_by_engine_settings_id',
    list_engine_settings_change_versions_request:
        'reporting.v1.engine_settings_changes_versions.list',
    get_engine_settings_change_version_request: 'reporting.v1.engine_settings_changes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_engine_settings_changes_request: true,
    get_engine_settings_change_request: true,
    get_many_engine_settings_changes_request: true,
    list_by_request_id_engine_settings_changes_request: true,
    list_by_engine_settings_id_engine_settings_changes_request: true,
    list_engine_settings_change_versions_request: true,
    get_engine_settings_change_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.engine_settings_changes_events.created',
    updated: 'reporting.v1.engine_settings_changes_events.updated',
    deleted: 'reporting.v1.engine_settings_changes_events.deleted',
} as const;
