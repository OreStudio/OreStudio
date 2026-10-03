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
import type { DefaultCurveConfig } from '../domain/default_curve_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DefaultCurveConfigKey {
    id: string;
}

export interface DefaultCurveConfigWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
}

export interface DefaultCurveConfigChange {
    write: DefaultCurveConfigWrite;
    precondition: Precondition;
}

export interface DefaultCurveConfigRemoval {
    key: DefaultCurveConfigKey;
    precondition: Precondition;
}

export interface DefaultCurveConfigLookup {
    key: DefaultCurveConfigKey;
    default_curve_config: DefaultCurveConfig | null;
}

export interface DefaultCurveConfigEvent {
    event_id: string;
    key: DefaultCurveConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DefaultCurveConfigVersionKey {
    default_curve_config: DefaultCurveConfigKey;
    version: number;
}

export interface DefaultCurveConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDefaultCurveConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListDefaultCurveConfigsResponse {
    result: Result;
    default_curve_configs: DefaultCurveConfig[];
    total: number;
}

export interface GetDefaultCurveConfigRequest {
    key: DefaultCurveConfigKey;
}

export interface GetDefaultCurveConfigResponse {
    result: Result;
    default_curve_config: DefaultCurveConfig | null;
}

export interface GetManyDefaultCurveConfigsRequest {
    keys: DefaultCurveConfigKey[];
}

export interface GetManyDefaultCurveConfigsResponse {
    result: Result;
    entries: DefaultCurveConfigLookup[];
}

export interface PutDefaultCurveConfigRequest {
    change: DefaultCurveConfigChange;
    intent: ChangeIntent;
}

export interface PutDefaultCurveConfigResponse {
    result: Result;
    default_curve_config: DefaultCurveConfig | null;
}

export interface PutManyDefaultCurveConfigsRequest {
    changes: DefaultCurveConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyDefaultCurveConfigsResponse {
    result: Result;
    default_curve_configs: DefaultCurveConfig[];
}

export interface DeleteDefaultCurveConfigRequest {
    removal: DefaultCurveConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteDefaultCurveConfigResponse {
    result: Result;
}

export interface DeleteManyDefaultCurveConfigsRequest {
    removals: DefaultCurveConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDefaultCurveConfigsResponse {
    result: Result;
}

export interface ListDefaultCurveConfigVersionsRequest {
    key: DefaultCurveConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveConfigVersionsFilter | null;
}

export interface ListDefaultCurveConfigVersionsResponse {
    result: Result;
    versions: DefaultCurveConfig[];
    total: number;
}

export interface GetDefaultCurveConfigVersionRequest {
    key: DefaultCurveConfigVersionKey;
}

export interface GetDefaultCurveConfigVersionResponse {
    result: Result;
    version: DefaultCurveConfig | null;
}

export const subjects = {
    list_default_curve_configs_request: 'refdata.v1.default_curve_configs.list',
    get_default_curve_config_request: 'refdata.v1.default_curve_configs.get',
    get_many_default_curve_configs_request: 'refdata.v1.default_curve_configs.get_many',
    put_default_curve_config_request: 'refdata.v1.default_curve_configs.put',
    put_many_default_curve_configs_request: 'refdata.v1.default_curve_configs.put_many',
    delete_default_curve_config_request: 'refdata.v1.default_curve_configs.delete',
    delete_many_default_curve_configs_request: 'refdata.v1.default_curve_configs.delete_many',
    list_default_curve_config_versions_request: 'refdata.v1.default_curve_configs_versions.list',
    get_default_curve_config_version_request: 'refdata.v1.default_curve_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_default_curve_configs_request: true,
    get_default_curve_config_request: true,
    get_many_default_curve_configs_request: true,
    put_default_curve_config_request: true,
    put_many_default_curve_configs_request: true,
    delete_default_curve_config_request: true,
    delete_many_default_curve_configs_request: true,
    list_default_curve_config_versions_request: true,
    get_default_curve_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.default_curve_configs_events.created',
    updated: 'refdata.v1.default_curve_configs_events.updated',
    deleted: 'refdata.v1.default_curve_configs_events.deleted',
} as const;
