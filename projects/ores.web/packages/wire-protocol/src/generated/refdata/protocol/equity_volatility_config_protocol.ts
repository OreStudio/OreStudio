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
import type { EquityVolatilityConfig } from '../domain/equity_volatility_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityVolatilityConfigKey {
    id: string;
}

export interface EquityVolatilityConfigWrite {
    id: string;
    curve_definition_id: string;
    equity_id: string | null;
    currency: string;
    dimension: string | null;
    expiries: string | null;
    strikes: string | null;
    day_counter: string | null;
    time_extrapolation: string | null;
    strike_extrapolation: string | null;
    calendar: string | null;
    prefer_out_of_the_money: string | null;
    has_volatility_config: boolean;
}

export interface EquityVolatilityConfigChange {
    write: EquityVolatilityConfigWrite;
    precondition: Precondition;
}

export interface EquityVolatilityConfigRemoval {
    key: EquityVolatilityConfigKey;
    precondition: Precondition;
}

export interface EquityVolatilityConfigLookup {
    key: EquityVolatilityConfigKey;
    equity_volatility_config: EquityVolatilityConfig | null;
}

export interface EquityVolatilityConfigEvent {
    event_id: string;
    key: EquityVolatilityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityVolatilityConfigVersionKey {
    equity_volatility_config: EquityVolatilityConfigKey;
    version: number;
}

export interface EquityVolatilityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityVolatilityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListEquityVolatilityConfigsResponse {
    result: Result;
    equity_volatility_configs: EquityVolatilityConfig[];
    total: number;
}

export interface GetEquityVolatilityConfigRequest {
    key: EquityVolatilityConfigKey;
}

export interface GetEquityVolatilityConfigResponse {
    result: Result;
    equity_volatility_config: EquityVolatilityConfig | null;
}

export interface GetManyEquityVolatilityConfigsRequest {
    keys: EquityVolatilityConfigKey[];
}

export interface GetManyEquityVolatilityConfigsResponse {
    result: Result;
    entries: EquityVolatilityConfigLookup[];
}

export interface PutEquityVolatilityConfigRequest {
    change: EquityVolatilityConfigChange;
    intent: ChangeIntent;
}

export interface PutEquityVolatilityConfigResponse {
    result: Result;
    equity_volatility_config: EquityVolatilityConfig | null;
}

export interface PutManyEquityVolatilityConfigsRequest {
    changes: EquityVolatilityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityVolatilityConfigsResponse {
    result: Result;
    equity_volatility_configs: EquityVolatilityConfig[];
}

export interface DeleteEquityVolatilityConfigRequest {
    removal: EquityVolatilityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityVolatilityConfigResponse {
    result: Result;
}

export interface DeleteManyEquityVolatilityConfigsRequest {
    removals: EquityVolatilityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityVolatilityConfigsResponse {
    result: Result;
}

export interface ListEquityVolatilityConfigVersionsRequest {
    key: EquityVolatilityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityVolatilityConfigVersionsFilter | null;
}

export interface ListEquityVolatilityConfigVersionsResponse {
    result: Result;
    versions: EquityVolatilityConfig[];
    total: number;
}

export interface GetEquityVolatilityConfigVersionRequest {
    key: EquityVolatilityConfigVersionKey;
}

export interface GetEquityVolatilityConfigVersionResponse {
    result: Result;
    version: EquityVolatilityConfig | null;
}

export const subjects = {
    list_equity_volatility_configs_request: 'refdata.v1.equity_volatility_configs.list',
    get_equity_volatility_config_request: 'refdata.v1.equity_volatility_configs.get',
    get_many_equity_volatility_configs_request: 'refdata.v1.equity_volatility_configs.get_many',
    put_equity_volatility_config_request: 'refdata.v1.equity_volatility_configs.put',
    put_many_equity_volatility_configs_request: 'refdata.v1.equity_volatility_configs.put_many',
    delete_equity_volatility_config_request: 'refdata.v1.equity_volatility_configs.delete',
    delete_many_equity_volatility_configs_request:
        'refdata.v1.equity_volatility_configs.delete_many',
    list_equity_volatility_config_versions_request:
        'refdata.v1.equity_volatility_configs_versions.list',
    get_equity_volatility_config_version_request:
        'refdata.v1.equity_volatility_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_volatility_configs_request: true,
    get_equity_volatility_config_request: true,
    get_many_equity_volatility_configs_request: true,
    put_equity_volatility_config_request: true,
    put_many_equity_volatility_configs_request: true,
    delete_equity_volatility_config_request: true,
    delete_many_equity_volatility_configs_request: true,
    list_equity_volatility_config_versions_request: true,
    get_equity_volatility_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.equity_volatility_configs_events.created',
    updated: 'refdata.v1.equity_volatility_configs_events.updated',
    deleted: 'refdata.v1.equity_volatility_configs_events.deleted',
} as const;
