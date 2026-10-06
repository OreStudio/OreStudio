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
import type { YieldVolatilityConfig } from '../domain/yield_volatility_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface YieldVolatilityConfigKey {
    id: string;
}

export interface YieldVolatilityConfigWrite {
    id: string;
    curve_definition_id: string;
    qualifier: string;
    dimension: string | null;
    volatility_type: string;
    extrapolation: string;
    day_counter: string;
    calendar: string;
    business_day_convention: string;
    option_tenors: string;
    bond_tenors: string;
}

export interface YieldVolatilityConfigChange {
    write: YieldVolatilityConfigWrite;
    precondition: Precondition;
}

export interface YieldVolatilityConfigRemoval {
    key: YieldVolatilityConfigKey;
    precondition: Precondition;
}

export interface YieldVolatilityConfigLookup {
    key: YieldVolatilityConfigKey;
    yield_volatility_config: YieldVolatilityConfig | null;
}

export interface YieldVolatilityConfigsFilter {
    id_one_of: string[] | null;
}

export interface YieldVolatilityConfigEvent {
    event_id: string;
    key: YieldVolatilityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface YieldVolatilityConfigVersionKey {
    yield_volatility_config: YieldVolatilityConfigKey;
    version: number;
}

export interface YieldVolatilityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListYieldVolatilityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: YieldVolatilityConfigsFilter | null;
    as_of: string | null;
}

export interface ListYieldVolatilityConfigsResponse {
    result: Result;
    yield_volatility_configs: YieldVolatilityConfig[];
    total: number;
}

export interface GetYieldVolatilityConfigRequest {
    key: YieldVolatilityConfigKey;
}

export interface GetYieldVolatilityConfigResponse {
    result: Result;
    yield_volatility_config: YieldVolatilityConfig | null;
}

export interface GetManyYieldVolatilityConfigsRequest {
    keys: YieldVolatilityConfigKey[];
}

export interface GetManyYieldVolatilityConfigsResponse {
    result: Result;
    entries: YieldVolatilityConfigLookup[];
}

export interface PutYieldVolatilityConfigRequest {
    change: YieldVolatilityConfigChange;
    intent: ChangeIntent;
}

export interface PutYieldVolatilityConfigResponse {
    result: Result;
    yield_volatility_config: YieldVolatilityConfig | null;
}

export interface PutManyYieldVolatilityConfigsRequest {
    changes: YieldVolatilityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyYieldVolatilityConfigsResponse {
    result: Result;
    yield_volatility_configs: YieldVolatilityConfig[];
}

export interface DeleteYieldVolatilityConfigRequest {
    removal: YieldVolatilityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteYieldVolatilityConfigResponse {
    result: Result;
}

export interface DeleteManyYieldVolatilityConfigsRequest {
    removals: YieldVolatilityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyYieldVolatilityConfigsResponse {
    result: Result;
}

export interface ListYieldVolatilityConfigVersionsRequest {
    key: YieldVolatilityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: YieldVolatilityConfigVersionsFilter | null;
}

export interface ListYieldVolatilityConfigVersionsResponse {
    result: Result;
    versions: YieldVolatilityConfig[];
    total: number;
}

export interface GetYieldVolatilityConfigVersionRequest {
    key: YieldVolatilityConfigVersionKey;
}

export interface GetYieldVolatilityConfigVersionResponse {
    result: Result;
    version: YieldVolatilityConfig | null;
}

export const subjects = {
    list_yield_volatility_configs_request: 'refdata.v1.yield_volatility_configs.list',
    get_yield_volatility_config_request: 'refdata.v1.yield_volatility_configs.get',
    get_many_yield_volatility_configs_request: 'refdata.v1.yield_volatility_configs.get_many',
    put_yield_volatility_config_request: 'refdata.v1.yield_volatility_configs.put',
    put_many_yield_volatility_configs_request: 'refdata.v1.yield_volatility_configs.put_many',
    delete_yield_volatility_config_request: 'refdata.v1.yield_volatility_configs.delete',
    delete_many_yield_volatility_configs_request: 'refdata.v1.yield_volatility_configs.delete_many',
    list_yield_volatility_config_versions_request:
        'refdata.v1.yield_volatility_configs_versions.list',
    get_yield_volatility_config_version_request: 'refdata.v1.yield_volatility_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_yield_volatility_configs_request: true,
    get_yield_volatility_config_request: true,
    get_many_yield_volatility_configs_request: true,
    put_yield_volatility_config_request: true,
    put_many_yield_volatility_configs_request: true,
    delete_yield_volatility_config_request: true,
    delete_many_yield_volatility_configs_request: true,
    list_yield_volatility_config_versions_request: true,
    get_yield_volatility_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.yield_volatility_configs_events.created',
    updated: 'refdata.v1.yield_volatility_configs_events.updated',
    deleted: 'refdata.v1.yield_volatility_configs_events.deleted',
} as const;
