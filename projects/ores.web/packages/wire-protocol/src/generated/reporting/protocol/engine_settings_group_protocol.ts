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
import type { EngineSettingsGroup } from '../domain/engine_settings_group.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EngineSettingsGroupKey {
    code: string;
}

export interface EngineSettingsGroupWrite {
    code: string;
    name: string;
    description: string;
    position: number;
}

export interface EngineSettingsGroupChange {
    write: EngineSettingsGroupWrite;
    precondition: Precondition;
}

export interface EngineSettingsGroupRemoval {
    key: EngineSettingsGroupKey;
    precondition: Precondition;
}

export interface EngineSettingsGroupLookup {
    key: EngineSettingsGroupKey;
    engine_settings_group: EngineSettingsGroup | null;
}

export interface EngineSettingsGroupsFilter {
    code_one_of: string[] | null;
}

export interface EngineSettingsGroupEvent {
    event_id: string;
    key: EngineSettingsGroupKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EngineSettingsGroupVersionKey {
    engine_settings_group: EngineSettingsGroupKey;
    version: number;
}

export interface EngineSettingsGroupVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEngineSettingsGroupsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsGroupsFilter | null;
    as_of: string | null;
}

export interface ListEngineSettingsGroupsResponse {
    result: Result;
    groups: EngineSettingsGroup[];
    total: number;
}

export interface GetEngineSettingsGroupRequest {
    key: EngineSettingsGroupKey;
}

export interface GetEngineSettingsGroupResponse {
    result: Result;
    engine_settings_group: EngineSettingsGroup | null;
}

export interface GetManyEngineSettingsGroupsRequest {
    keys: EngineSettingsGroupKey[];
}

export interface GetManyEngineSettingsGroupsResponse {
    result: Result;
    entries: EngineSettingsGroupLookup[];
}

export interface PutEngineSettingsGroupRequest {
    change: EngineSettingsGroupChange;
    intent: ChangeIntent;
}

export interface PutEngineSettingsGroupResponse {
    result: Result;
    engine_settings_group: EngineSettingsGroup | null;
}

export interface PutManyEngineSettingsGroupsRequest {
    changes: EngineSettingsGroupChange[];
    intent: ChangeIntent;
}

export interface PutManyEngineSettingsGroupsResponse {
    result: Result;
    groups: EngineSettingsGroup[];
}

export interface DeleteEngineSettingsGroupRequest {
    removal: EngineSettingsGroupRemoval;
    intent: ChangeIntent;
}

export interface DeleteEngineSettingsGroupResponse {
    result: Result;
}

export interface DeleteManyEngineSettingsGroupsRequest {
    removals: EngineSettingsGroupRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEngineSettingsGroupsResponse {
    result: Result;
}

export interface ListEngineSettingsGroupVersionsRequest {
    key: EngineSettingsGroupKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsGroupVersionsFilter | null;
}

export interface ListEngineSettingsGroupVersionsResponse {
    result: Result;
    versions: EngineSettingsGroup[];
    total: number;
}

export interface GetEngineSettingsGroupVersionRequest {
    key: EngineSettingsGroupVersionKey;
}

export interface GetEngineSettingsGroupVersionResponse {
    result: Result;
    version: EngineSettingsGroup | null;
}

export const subjects = {
    list_engine_settings_groups_request: 'reporting.v1.engine_settings_groups.list',
    get_engine_settings_group_request: 'reporting.v1.engine_settings_groups.get',
    get_many_engine_settings_groups_request: 'reporting.v1.engine_settings_groups.get_many',
    put_engine_settings_group_request: 'reporting.v1.engine_settings_groups.put',
    put_many_engine_settings_groups_request: 'reporting.v1.engine_settings_groups.put_many',
    delete_engine_settings_group_request: 'reporting.v1.engine_settings_groups.delete',
    delete_many_engine_settings_groups_request: 'reporting.v1.engine_settings_groups.delete_many',
    list_engine_settings_group_versions_request:
        'reporting.v1.engine_settings_groups_versions.list',
    get_engine_settings_group_version_request: 'reporting.v1.engine_settings_groups_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_engine_settings_groups_request: true,
    get_engine_settings_group_request: true,
    get_many_engine_settings_groups_request: true,
    put_engine_settings_group_request: true,
    put_many_engine_settings_groups_request: true,
    delete_engine_settings_group_request: true,
    delete_many_engine_settings_groups_request: true,
    list_engine_settings_group_versions_request: true,
    get_engine_settings_group_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.engine_settings_groups_events.created',
    updated: 'reporting.v1.engine_settings_groups_events.updated',
    deleted: 'reporting.v1.engine_settings_groups_events.deleted',
} as const;
