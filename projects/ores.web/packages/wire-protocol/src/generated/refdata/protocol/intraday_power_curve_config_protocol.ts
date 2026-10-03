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
import type { IntradayPowerCurveConfig } from '../domain/intraday_power_curve_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IntradayPowerCurveConfigKey {
    id: string;
}

export interface IntradayPowerCurveConfigWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
    daily_average_price_curve: string;
    shape_quote_name: string;
    convention: string | null;
}

export interface IntradayPowerCurveConfigChange {
    write: IntradayPowerCurveConfigWrite;
    precondition: Precondition;
}

export interface IntradayPowerCurveConfigRemoval {
    key: IntradayPowerCurveConfigKey;
    precondition: Precondition;
}

export interface IntradayPowerCurveConfigLookup {
    key: IntradayPowerCurveConfigKey;
    intraday_power_curve_config: IntradayPowerCurveConfig | null;
}

export interface IntradayPowerCurveConfigEvent {
    event_id: string;
    key: IntradayPowerCurveConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IntradayPowerCurveConfigVersionKey {
    intraday_power_curve_config: IntradayPowerCurveConfigKey;
    version: number;
}

export interface IntradayPowerCurveConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIntradayPowerCurveConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListIntradayPowerCurveConfigsResponse {
    result: Result;
    intraday_power_curve_configs: IntradayPowerCurveConfig[];
    total: number;
}

export interface GetIntradayPowerCurveConfigRequest {
    key: IntradayPowerCurveConfigKey;
}

export interface GetIntradayPowerCurveConfigResponse {
    result: Result;
    intraday_power_curve_config: IntradayPowerCurveConfig | null;
}

export interface GetManyIntradayPowerCurveConfigsRequest {
    keys: IntradayPowerCurveConfigKey[];
}

export interface GetManyIntradayPowerCurveConfigsResponse {
    result: Result;
    entries: IntradayPowerCurveConfigLookup[];
}

export interface PutIntradayPowerCurveConfigRequest {
    change: IntradayPowerCurveConfigChange;
    intent: ChangeIntent;
}

export interface PutIntradayPowerCurveConfigResponse {
    result: Result;
    intraday_power_curve_config: IntradayPowerCurveConfig | null;
}

export interface PutManyIntradayPowerCurveConfigsRequest {
    changes: IntradayPowerCurveConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyIntradayPowerCurveConfigsResponse {
    result: Result;
    intraday_power_curve_configs: IntradayPowerCurveConfig[];
}

export interface DeleteIntradayPowerCurveConfigRequest {
    removal: IntradayPowerCurveConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteIntradayPowerCurveConfigResponse {
    result: Result;
}

export interface DeleteManyIntradayPowerCurveConfigsRequest {
    removals: IntradayPowerCurveConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIntradayPowerCurveConfigsResponse {
    result: Result;
}

export interface ListIntradayPowerCurveConfigVersionsRequest {
    key: IntradayPowerCurveConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IntradayPowerCurveConfigVersionsFilter | null;
}

export interface ListIntradayPowerCurveConfigVersionsResponse {
    result: Result;
    versions: IntradayPowerCurveConfig[];
    total: number;
}

export interface GetIntradayPowerCurveConfigVersionRequest {
    key: IntradayPowerCurveConfigVersionKey;
}

export interface GetIntradayPowerCurveConfigVersionResponse {
    result: Result;
    version: IntradayPowerCurveConfig | null;
}

export const subjects = {
    list_intraday_power_curve_configs_request: 'refdata.v1.intraday_power_curve_configs.list',
    get_intraday_power_curve_config_request: 'refdata.v1.intraday_power_curve_configs.get',
    get_many_intraday_power_curve_configs_request:
        'refdata.v1.intraday_power_curve_configs.get_many',
    put_intraday_power_curve_config_request: 'refdata.v1.intraday_power_curve_configs.put',
    put_many_intraday_power_curve_configs_request:
        'refdata.v1.intraday_power_curve_configs.put_many',
    delete_intraday_power_curve_config_request: 'refdata.v1.intraday_power_curve_configs.delete',
    delete_many_intraday_power_curve_configs_request:
        'refdata.v1.intraday_power_curve_configs.delete_many',
    list_intraday_power_curve_config_versions_request:
        'refdata.v1.intraday_power_curve_configs_versions.list',
    get_intraday_power_curve_config_version_request:
        'refdata.v1.intraday_power_curve_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_intraday_power_curve_configs_request: true,
    get_intraday_power_curve_config_request: true,
    get_many_intraday_power_curve_configs_request: true,
    put_intraday_power_curve_config_request: true,
    put_many_intraday_power_curve_configs_request: true,
    delete_intraday_power_curve_config_request: true,
    delete_many_intraday_power_curve_configs_request: true,
    list_intraday_power_curve_config_versions_request: true,
    get_intraday_power_curve_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.intraday_power_curve_configs_events.created',
    updated: 'refdata.v1.intraday_power_curve_configs_events.updated',
    deleted: 'refdata.v1.intraday_power_curve_configs_events.deleted',
} as const;
