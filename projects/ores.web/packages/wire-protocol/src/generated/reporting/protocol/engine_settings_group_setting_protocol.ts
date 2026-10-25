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
import type { EngineSettingsGroupSetting } from '../domain/engine_settings_group_setting.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EngineSettingsGroupSettingKey {
    code: string;
}

export interface EngineSettingsGroupSettingWrite {
    code: string;
    group_code: string;
    column_name: string;
    ore_name: string;
    kind: string;
    changes_numbers: boolean;
    options: string;
    minimum: number | null;
    maximum: number | null;
    position: number;
}

export interface EngineSettingsGroupSettingChange {
    write: EngineSettingsGroupSettingWrite;
    precondition: Precondition;
}

export interface EngineSettingsGroupSettingRemoval {
    key: EngineSettingsGroupSettingKey;
    precondition: Precondition;
}

export interface EngineSettingsGroupSettingLookup {
    key: EngineSettingsGroupSettingKey;
    engine_settings_group_setting: EngineSettingsGroupSetting | null;
}

export interface EngineSettingsGroupSettingsFilter {
    code_one_of: string[] | null;
}

export interface EngineSettingsGroupSettingEvent {
    event_id: string;
    key: EngineSettingsGroupSettingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EngineSettingsGroupSettingVersionKey {
    engine_settings_group_setting: EngineSettingsGroupSettingKey;
    version: number;
}

export interface EngineSettingsGroupSettingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEngineSettingsGroupSettingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsGroupSettingsFilter | null;
    as_of: string | null;
}

export interface ListEngineSettingsGroupSettingsResponse {
    result: Result;
    settings: EngineSettingsGroupSetting[];
    total: number;
}

export interface GetEngineSettingsGroupSettingRequest {
    key: EngineSettingsGroupSettingKey;
}

export interface GetEngineSettingsGroupSettingResponse {
    result: Result;
    engine_settings_group_setting: EngineSettingsGroupSetting | null;
}

export interface GetManyEngineSettingsGroupSettingsRequest {
    keys: EngineSettingsGroupSettingKey[];
}

export interface GetManyEngineSettingsGroupSettingsResponse {
    result: Result;
    entries: EngineSettingsGroupSettingLookup[];
}

export interface PutEngineSettingsGroupSettingRequest {
    change: EngineSettingsGroupSettingChange;
    intent: ChangeIntent;
}

export interface PutEngineSettingsGroupSettingResponse {
    result: Result;
    engine_settings_group_setting: EngineSettingsGroupSetting | null;
}

export interface PutManyEngineSettingsGroupSettingsRequest {
    changes: EngineSettingsGroupSettingChange[];
    intent: ChangeIntent;
}

export interface PutManyEngineSettingsGroupSettingsResponse {
    result: Result;
    settings: EngineSettingsGroupSetting[];
}

export interface DeleteEngineSettingsGroupSettingRequest {
    removal: EngineSettingsGroupSettingRemoval;
    intent: ChangeIntent;
}

export interface DeleteEngineSettingsGroupSettingResponse {
    result: Result;
}

export interface DeleteManyEngineSettingsGroupSettingsRequest {
    removals: EngineSettingsGroupSettingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEngineSettingsGroupSettingsResponse {
    result: Result;
}

export interface ListEngineSettingsGroupSettingVersionsRequest {
    key: EngineSettingsGroupSettingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsGroupSettingVersionsFilter | null;
}

export interface ListEngineSettingsGroupSettingVersionsResponse {
    result: Result;
    versions: EngineSettingsGroupSetting[];
    total: number;
}

export interface GetEngineSettingsGroupSettingVersionRequest {
    key: EngineSettingsGroupSettingVersionKey;
}

export interface GetEngineSettingsGroupSettingVersionResponse {
    result: Result;
    version: EngineSettingsGroupSetting | null;
}

export const subjects = {
    list_engine_settings_group_settings_request: 'reporting.v1.engine_settings_group_settings.list',
    get_engine_settings_group_setting_request: 'reporting.v1.engine_settings_group_settings.get',
    get_many_engine_settings_group_settings_request:
        'reporting.v1.engine_settings_group_settings.get_many',
    put_engine_settings_group_setting_request: 'reporting.v1.engine_settings_group_settings.put',
    put_many_engine_settings_group_settings_request:
        'reporting.v1.engine_settings_group_settings.put_many',
    delete_engine_settings_group_setting_request:
        'reporting.v1.engine_settings_group_settings.delete',
    delete_many_engine_settings_group_settings_request:
        'reporting.v1.engine_settings_group_settings.delete_many',
    list_engine_settings_group_setting_versions_request:
        'reporting.v1.engine_settings_group_settings_versions.list',
    get_engine_settings_group_setting_version_request:
        'reporting.v1.engine_settings_group_settings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_engine_settings_group_settings_request: true,
    get_engine_settings_group_setting_request: true,
    get_many_engine_settings_group_settings_request: true,
    put_engine_settings_group_setting_request: true,
    put_many_engine_settings_group_settings_request: true,
    delete_engine_settings_group_setting_request: true,
    delete_many_engine_settings_group_settings_request: true,
    list_engine_settings_group_setting_versions_request: true,
    get_engine_settings_group_setting_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.engine_settings_group_settings_events.created',
    updated: 'reporting.v1.engine_settings_group_settings_events.updated',
    deleted: 'reporting.v1.engine_settings_group_settings_events.deleted',
} as const;
