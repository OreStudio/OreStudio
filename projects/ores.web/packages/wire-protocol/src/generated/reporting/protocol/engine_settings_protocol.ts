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
import type { EngineSettings } from '../domain/engine_settings.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface EngineSettingsKey {
    name: string;
}

export interface EngineSettingsWrite {
    id: string;
    party_id: string;
    name: string;
    description: string;
    configuration_id: string;
}

export interface EngineSettingsChange {
    write: EngineSettingsWrite;
    precondition: Precondition;
}

export interface EngineSettingsRemoval {
    key: EngineSettingsKey;
    precondition: Precondition;
}

export interface EngineSettingsLookup {
    key: EngineSettingsKey;
    engine_settings: EngineSettings | null;
}

export interface EngineSettingsFilter {
    configuration_id: string | null;
    id_one_of: string[] | null;
    configuration_id_one_of: string[] | null;
}

export interface EngineSettingsEvent {
    event_id: string;
    key: EngineSettingsKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EngineSettingsVersionKey {
    engine_settings: EngineSettingsKey;
    version: number;
}

export interface EngineSettingsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEngineSettingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsFilter | null;
    as_of: string | null;
}

export interface ListEngineSettingsResponse {
    result: Result;
    settings: EngineSettings[];
    total: number;
}

export interface GetEngineSettingsRequest {
    key: EngineSettingsKey;
}

export interface GetEngineSettingsResponse {
    result: Result;
    engine_settings: EngineSettings | null;
}

export interface GetManyEngineSettingsRequest {
    keys: EngineSettingsKey[];
}

export interface GetManyEngineSettingsResponse {
    result: Result;
    entries: EngineSettingsLookup[];
}

export interface PutEngineSettingsRequest {
    change: EngineSettingsChange;
    intent: ChangeIntent;
}

export interface PutEngineSettingsResponse {
    result: Result;
    engine_settings: EngineSettings | null;
}

export interface PutManyEngineSettingsRequest {
    changes: EngineSettingsChange[];
    intent: ChangeIntent;
}

export interface PutManyEngineSettingsResponse {
    result: Result;
    settings: EngineSettings[];
}

export interface DeleteEngineSettingsRequest {
    removal: EngineSettingsRemoval;
    intent: ChangeIntent;
}

export interface DeleteEngineSettingsResponse {
    result: Result;
}

export interface DeleteManyEngineSettingsRequest {
    removals: EngineSettingsRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEngineSettingsResponse {
    result: Result;
}

export interface ListByConfigurationIdEngineSettingsRequest {
    configuration_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsFilter | null;
}

export interface ListByConfigurationIdEngineSettingsResponse {
    result: Result;
    settings: EngineSettings[];
    total: number;
}

export interface ListEngineSettingsVersionsRequest {
    key: EngineSettingsKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsVersionsFilter | null;
}

export interface ListEngineSettingsVersionsResponse {
    result: Result;
    versions: EngineSettings[];
    total: number;
}

export interface GetEngineSettingsVersionRequest {
    key: EngineSettingsVersionKey;
}

export interface GetEngineSettingsVersionResponse {
    result: Result;
    version: EngineSettings | null;
}

export const subjects = {
    list_engine_settings_request: 'reporting.v1.engine_settings.list',
    get_engine_settings_request: 'reporting.v1.engine_settings.get',
    get_many_engine_settings_request: 'reporting.v1.engine_settings.get_many',
    put_engine_settings_request: 'reporting.v1.engine_settings.put',
    put_many_engine_settings_request: 'reporting.v1.engine_settings.put_many',
    delete_engine_settings_request: 'reporting.v1.engine_settings.delete',
    delete_many_engine_settings_request: 'reporting.v1.engine_settings.delete_many',
    list_by_configuration_id_engine_settings_request:
        'reporting.v1.engine_settings.list_by_configuration_id',
    list_engine_settings_versions_request: 'reporting.v1.engine_settings_versions.list',
    get_engine_settings_version_request: 'reporting.v1.engine_settings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_engine_settings_request: true,
    get_engine_settings_request: true,
    get_many_engine_settings_request: true,
    put_engine_settings_request: true,
    put_many_engine_settings_request: true,
    delete_engine_settings_request: true,
    delete_many_engine_settings_request: true,
    list_by_configuration_id_engine_settings_request: true,
    list_engine_settings_versions_request: true,
    get_engine_settings_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.engine_settings_events.created',
    updated: 'reporting.v1.engine_settings_events.updated',
    deleted: 'reporting.v1.engine_settings_events.deleted',
} as const;
