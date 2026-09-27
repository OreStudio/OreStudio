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
import type { SystemSetting } from '../domain/system_setting.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SystemSettingKey {
    name: string;
}

export interface SystemSettingWrite {
    id: string;
    name: string;
    party_id: string;
    value: string;
    data_type: string;
    description: string;
}

export interface SystemSettingChange {
    write: SystemSettingWrite;
    precondition: Precondition;
}

export interface SystemSettingRemoval {
    key: SystemSettingKey;
    precondition: Precondition;
}

export interface SystemSettingLookup {
    key: SystemSettingKey;
    system_setting: SystemSetting | null;
}

export interface SystemSettingEvent {
    event_id: string;
    key: SystemSettingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SystemSettingVersionKey {
    system_setting: SystemSettingKey;
    version: number;
}

export interface SystemSettingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSystemSettingsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSystemSettingsResponse {
    result: Result;
    settings: SystemSetting[];
    total: number;
}

export interface GetSystemSettingRequest {
    key: SystemSettingKey;
}

export interface GetSystemSettingResponse {
    result: Result;
    system_setting: SystemSetting | null;
}

export interface GetManySystemSettingsRequest {
    keys: SystemSettingKey[];
}

export interface GetManySystemSettingsResponse {
    result: Result;
    entries: SystemSettingLookup[];
}

export interface PutSystemSettingRequest {
    change: SystemSettingChange;
    intent: ChangeIntent;
}

export interface PutSystemSettingResponse {
    result: Result;
    system_setting: SystemSetting;
}

export interface PutManySystemSettingsRequest {
    changes: SystemSettingChange[];
    intent: ChangeIntent;
}

export interface PutManySystemSettingsResponse {
    result: Result;
    settings: SystemSetting[];
}

export interface DeleteSystemSettingRequest {
    removal: SystemSettingRemoval;
    intent: ChangeIntent;
}

export interface DeleteSystemSettingResponse {
    result: Result;
}

export interface DeleteManySystemSettingsRequest {
    removals: SystemSettingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySystemSettingsResponse {
    result: Result;
}

export interface ListSystemSettingVersionsRequest {
    key: SystemSettingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SystemSettingVersionsFilter | null;
}

export interface ListSystemSettingVersionsResponse {
    result: Result;
    versions: SystemSetting[];
    total: number;
}

export interface GetSystemSettingVersionRequest {
    key: SystemSettingVersionKey;
}

export interface GetSystemSettingVersionResponse {
    result: Result;
    version: SystemSetting;
}

export const subjects = {
    list_system_settings_request: 'variability.v1.system_settings.list',
    get_system_setting_request: 'variability.v1.system_settings.get',
    get_many_system_settings_request: 'variability.v1.system_settings.get_many',
    put_system_setting_request: 'variability.v1.system_settings.put',
    put_many_system_settings_request: 'variability.v1.system_settings.put_many',
    delete_system_setting_request: 'variability.v1.system_settings.delete',
    delete_many_system_settings_request: 'variability.v1.system_settings.delete_many',
    list_system_setting_versions_request: 'variability.v1.system_settings_versions.list',
    get_system_setting_version_request: 'variability.v1.system_settings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_system_settings_request: true,
    get_system_setting_request: true,
    get_many_system_settings_request: true,
    put_system_setting_request: true,
    put_many_system_settings_request: true,
    delete_system_setting_request: true,
    delete_many_system_settings_request: true,
    list_system_setting_versions_request: true,
    get_system_setting_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'variability.v1.system_settings_events.created',
    updated: 'variability.v1.system_settings_events.updated',
    deleted: 'variability.v1.system_settings_events.deleted',
} as const;
