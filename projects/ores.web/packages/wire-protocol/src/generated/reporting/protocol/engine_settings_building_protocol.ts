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
import type { EngineSettingsBuilding } from '../domain/engine_settings_building.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface EngineSettingsBuildingKey {
    id: string;
}

export interface EngineSettingsBuildingWrite {
    id: string;
    engine_settings_id: string;
    observation_model: string | null;
    lazy_market_building: boolean | null;
    build_failed_trades: boolean | null;
    continue_on_error: boolean | null;
    use_at_par_coupons_curves: boolean | null;
    use_at_par_coupons_trades: boolean | null;
}

export interface EngineSettingsBuildingChange {
    write: EngineSettingsBuildingWrite;
    precondition: Precondition;
}

export interface EngineSettingsBuildingRemoval {
    key: EngineSettingsBuildingKey;
    precondition: Precondition;
}

export interface EngineSettingsBuildingLookup {
    key: EngineSettingsBuildingKey;
    engine_settings_building: EngineSettingsBuilding | null;
}

export interface EngineSettingsBuildingFilter {
    engine_settings_id: string | null;
    id_one_of: string[] | null;
    engine_settings_id_one_of: string[] | null;
}

export interface EngineSettingsBuildingEvent {
    event_id: string;
    key: EngineSettingsBuildingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EngineSettingsBuildingVersionKey {
    engine_settings_building: EngineSettingsBuildingKey;
    version: number;
}

export interface EngineSettingsBuildingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEngineSettingsBuildingRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsBuildingFilter | null;
    as_of: string | null;
}

export interface ListEngineSettingsBuildingResponse {
    result: Result;
    engine_settings_building: EngineSettingsBuilding[];
    total: number;
}

export interface GetEngineSettingsBuildingRequest {
    key: EngineSettingsBuildingKey;
}

export interface GetEngineSettingsBuildingResponse {
    result: Result;
    engine_settings_building: EngineSettingsBuilding | null;
}

export interface GetManyEngineSettingsBuildingRequest {
    keys: EngineSettingsBuildingKey[];
}

export interface GetManyEngineSettingsBuildingResponse {
    result: Result;
    entries: EngineSettingsBuildingLookup[];
}

export interface PutEngineSettingsBuildingRequest {
    change: EngineSettingsBuildingChange;
    intent: ChangeIntent;
}

export interface PutEngineSettingsBuildingResponse {
    result: Result;
    engine_settings_building: EngineSettingsBuilding | null;
}

export interface PutManyEngineSettingsBuildingRequest {
    changes: EngineSettingsBuildingChange[];
    intent: ChangeIntent;
}

export interface PutManyEngineSettingsBuildingResponse {
    result: Result;
    engine_settings_building: EngineSettingsBuilding[];
}

export interface DeleteEngineSettingsBuildingRequest {
    removal: EngineSettingsBuildingRemoval;
    intent: ChangeIntent;
}

export interface DeleteEngineSettingsBuildingResponse {
    result: Result;
}

export interface DeleteManyEngineSettingsBuildingRequest {
    removals: EngineSettingsBuildingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEngineSettingsBuildingResponse {
    result: Result;
}

export interface ListByEngineSettingsIdEngineSettingsBuildingRequest {
    engine_settings_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsBuildingFilter | null;
}

export interface ListByEngineSettingsIdEngineSettingsBuildingResponse {
    result: Result;
    engine_settings_building: EngineSettingsBuilding[];
    total: number;
}

export interface ListEngineSettingsBuildingVersionsRequest {
    key: EngineSettingsBuildingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EngineSettingsBuildingVersionsFilter | null;
}

export interface ListEngineSettingsBuildingVersionsResponse {
    result: Result;
    versions: EngineSettingsBuilding[];
    total: number;
}

export interface GetEngineSettingsBuildingVersionRequest {
    key: EngineSettingsBuildingVersionKey;
}

export interface GetEngineSettingsBuildingVersionResponse {
    result: Result;
    version: EngineSettingsBuilding | null;
}

export const subjects = {
    list_engine_settings_building_request: 'reporting.v1.engine_settings_building.list',
    get_engine_settings_building_request: 'reporting.v1.engine_settings_building.get',
    get_many_engine_settings_building_request: 'reporting.v1.engine_settings_building.get_many',
    put_engine_settings_building_request: 'reporting.v1.engine_settings_building.put',
    put_many_engine_settings_building_request: 'reporting.v1.engine_settings_building.put_many',
    delete_engine_settings_building_request: 'reporting.v1.engine_settings_building.delete',
    delete_many_engine_settings_building_request:
        'reporting.v1.engine_settings_building.delete_many',
    list_by_engine_settings_id_engine_settings_building_request:
        'reporting.v1.engine_settings_building.list_by_engine_settings_id',
    list_engine_settings_building_versions_request:
        'reporting.v1.engine_settings_building_versions.list',
    get_engine_settings_building_version_request:
        'reporting.v1.engine_settings_building_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_engine_settings_building_request: true,
    get_engine_settings_building_request: true,
    get_many_engine_settings_building_request: true,
    put_engine_settings_building_request: true,
    put_many_engine_settings_building_request: true,
    delete_engine_settings_building_request: true,
    delete_many_engine_settings_building_request: true,
    list_by_engine_settings_id_engine_settings_building_request: true,
    list_engine_settings_building_versions_request: true,
    get_engine_settings_building_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.engine_settings_building_events.created',
    updated: 'reporting.v1.engine_settings_building_events.updated',
    deleted: 'reporting.v1.engine_settings_building_events.deleted',
} as const;
