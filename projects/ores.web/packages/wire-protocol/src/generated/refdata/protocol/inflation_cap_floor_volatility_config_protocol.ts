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
import type { InflationCapFloorVolatilityConfig } from '../domain/inflation_cap_floor_volatility_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InflationCapFloorVolatilityConfigKey {
    id: string;
}

export interface InflationCapFloorVolatilityConfigWrite {
    id: string;
    curve_definition_id: string;
    inflation_type: string;
    quote_type: string;
    volatility_type: string;
    extrapolation: string;
    tenors: string;
    settlement_days: number | null;
    cap_strikes: string | null;
    floor_strikes: string | null;
    strikes: string | null;
    calendar: string;
    day_counter: string;
    business_day_convention: string;
    index: string;
    index_curve: string;
    index_interpolated: string | null;
    observation_lag: string;
    yield_term_structure: string;
    quote_index: string | null;
    conventions: string | null;
}

export interface InflationCapFloorVolatilityConfigChange {
    write: InflationCapFloorVolatilityConfigWrite;
    precondition: Precondition;
}

export interface InflationCapFloorVolatilityConfigRemoval {
    key: InflationCapFloorVolatilityConfigKey;
    precondition: Precondition;
}

export interface InflationCapFloorVolatilityConfigLookup {
    key: InflationCapFloorVolatilityConfigKey;
    inflation_cap_floor_volatility_config: InflationCapFloorVolatilityConfig | null;
}

export interface InflationCapFloorVolatilityConfigsFilter {
    id_one_of: string[] | null;
}

export interface InflationCapFloorVolatilityConfigEvent {
    event_id: string;
    key: InflationCapFloorVolatilityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InflationCapFloorVolatilityConfigVersionKey {
    inflation_cap_floor_volatility_config: InflationCapFloorVolatilityConfigKey;
    version: number;
}

export interface InflationCapFloorVolatilityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInflationCapFloorVolatilityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InflationCapFloorVolatilityConfigsFilter | null;
}

export interface ListInflationCapFloorVolatilityConfigsResponse {
    result: Result;
    inflation_cap_floor_volatility_configs: InflationCapFloorVolatilityConfig[];
    total: number;
}

export interface GetInflationCapFloorVolatilityConfigRequest {
    key: InflationCapFloorVolatilityConfigKey;
}

export interface GetInflationCapFloorVolatilityConfigResponse {
    result: Result;
    inflation_cap_floor_volatility_config: InflationCapFloorVolatilityConfig | null;
}

export interface GetManyInflationCapFloorVolatilityConfigsRequest {
    keys: InflationCapFloorVolatilityConfigKey[];
}

export interface GetManyInflationCapFloorVolatilityConfigsResponse {
    result: Result;
    entries: InflationCapFloorVolatilityConfigLookup[];
}

export interface PutInflationCapFloorVolatilityConfigRequest {
    change: InflationCapFloorVolatilityConfigChange;
    intent: ChangeIntent;
}

export interface PutInflationCapFloorVolatilityConfigResponse {
    result: Result;
    inflation_cap_floor_volatility_config: InflationCapFloorVolatilityConfig | null;
}

export interface PutManyInflationCapFloorVolatilityConfigsRequest {
    changes: InflationCapFloorVolatilityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyInflationCapFloorVolatilityConfigsResponse {
    result: Result;
    inflation_cap_floor_volatility_configs: InflationCapFloorVolatilityConfig[];
}

export interface DeleteInflationCapFloorVolatilityConfigRequest {
    removal: InflationCapFloorVolatilityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteInflationCapFloorVolatilityConfigResponse {
    result: Result;
}

export interface DeleteManyInflationCapFloorVolatilityConfigsRequest {
    removals: InflationCapFloorVolatilityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInflationCapFloorVolatilityConfigsResponse {
    result: Result;
}

export interface ListInflationCapFloorVolatilityConfigVersionsRequest {
    key: InflationCapFloorVolatilityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InflationCapFloorVolatilityConfigVersionsFilter | null;
}

export interface ListInflationCapFloorVolatilityConfigVersionsResponse {
    result: Result;
    versions: InflationCapFloorVolatilityConfig[];
    total: number;
}

export interface GetInflationCapFloorVolatilityConfigVersionRequest {
    key: InflationCapFloorVolatilityConfigVersionKey;
}

export interface GetInflationCapFloorVolatilityConfigVersionResponse {
    result: Result;
    version: InflationCapFloorVolatilityConfig | null;
}

export const subjects = {
    list_inflation_cap_floor_volatility_configs_request:
        'refdata.v1.inflation_cap_floor_volatility_configs.list',
    get_inflation_cap_floor_volatility_config_request:
        'refdata.v1.inflation_cap_floor_volatility_configs.get',
    get_many_inflation_cap_floor_volatility_configs_request:
        'refdata.v1.inflation_cap_floor_volatility_configs.get_many',
    put_inflation_cap_floor_volatility_config_request:
        'refdata.v1.inflation_cap_floor_volatility_configs.put',
    put_many_inflation_cap_floor_volatility_configs_request:
        'refdata.v1.inflation_cap_floor_volatility_configs.put_many',
    delete_inflation_cap_floor_volatility_config_request:
        'refdata.v1.inflation_cap_floor_volatility_configs.delete',
    delete_many_inflation_cap_floor_volatility_configs_request:
        'refdata.v1.inflation_cap_floor_volatility_configs.delete_many',
    list_inflation_cap_floor_volatility_config_versions_request:
        'refdata.v1.inflation_cap_floor_volatility_configs_versions.list',
    get_inflation_cap_floor_volatility_config_version_request:
        'refdata.v1.inflation_cap_floor_volatility_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_inflation_cap_floor_volatility_configs_request: true,
    get_inflation_cap_floor_volatility_config_request: true,
    get_many_inflation_cap_floor_volatility_configs_request: true,
    put_inflation_cap_floor_volatility_config_request: true,
    put_many_inflation_cap_floor_volatility_configs_request: true,
    delete_inflation_cap_floor_volatility_config_request: true,
    delete_many_inflation_cap_floor_volatility_configs_request: true,
    list_inflation_cap_floor_volatility_config_versions_request: true,
    get_inflation_cap_floor_volatility_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.inflation_cap_floor_volatility_configs_events.created',
    updated: 'refdata.v1.inflation_cap_floor_volatility_configs_events.updated',
    deleted: 'refdata.v1.inflation_cap_floor_volatility_configs_events.deleted',
} as const;
