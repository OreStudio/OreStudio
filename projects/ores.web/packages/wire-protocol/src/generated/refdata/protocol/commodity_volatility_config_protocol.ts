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
import type { CommodityVolatilityConfig } from '../domain/commodity_volatility_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityVolatilityConfigKey {
    id: string;
}

export interface CommodityVolatilityConfigWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
    instrument_type: string | null;
    calendar_spread_offset: number | null;
    calendar_spread_underlying_name: string | null;
    day_counter: string | null;
    calendar: string | null;
    future_conventions: string | null;
    option_expiry_roll_days: string | null;
    price_curve_id: string | null;
    yield_curve_id: string | null;
    quote_suffix: string | null;
    prefer_out_of_the_money: string | null;
    has_volatility_config: boolean;
}

export interface CommodityVolatilityConfigChange {
    write: CommodityVolatilityConfigWrite;
    precondition: Precondition;
}

export interface CommodityVolatilityConfigRemoval {
    key: CommodityVolatilityConfigKey;
    precondition: Precondition;
}

export interface CommodityVolatilityConfigLookup {
    key: CommodityVolatilityConfigKey;
    commodity_volatility_config: CommodityVolatilityConfig | null;
}

export interface CommodityVolatilityConfigEvent {
    event_id: string;
    key: CommodityVolatilityConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityVolatilityConfigVersionKey {
    commodity_volatility_config: CommodityVolatilityConfigKey;
    version: number;
}

export interface CommodityVolatilityConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityVolatilityConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCommodityVolatilityConfigsResponse {
    result: Result;
    commodity_volatility_configs: CommodityVolatilityConfig[];
    total: number;
}

export interface GetCommodityVolatilityConfigRequest {
    key: CommodityVolatilityConfigKey;
}

export interface GetCommodityVolatilityConfigResponse {
    result: Result;
    commodity_volatility_config: CommodityVolatilityConfig | null;
}

export interface GetManyCommodityVolatilityConfigsRequest {
    keys: CommodityVolatilityConfigKey[];
}

export interface GetManyCommodityVolatilityConfigsResponse {
    result: Result;
    entries: CommodityVolatilityConfigLookup[];
}

export interface PutCommodityVolatilityConfigRequest {
    change: CommodityVolatilityConfigChange;
    intent: ChangeIntent;
}

export interface PutCommodityVolatilityConfigResponse {
    result: Result;
    commodity_volatility_config: CommodityVolatilityConfig | null;
}

export interface PutManyCommodityVolatilityConfigsRequest {
    changes: CommodityVolatilityConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityVolatilityConfigsResponse {
    result: Result;
    commodity_volatility_configs: CommodityVolatilityConfig[];
}

export interface DeleteCommodityVolatilityConfigRequest {
    removal: CommodityVolatilityConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityVolatilityConfigResponse {
    result: Result;
}

export interface DeleteManyCommodityVolatilityConfigsRequest {
    removals: CommodityVolatilityConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityVolatilityConfigsResponse {
    result: Result;
}

export interface ListCommodityVolatilityConfigVersionsRequest {
    key: CommodityVolatilityConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityVolatilityConfigVersionsFilter | null;
}

export interface ListCommodityVolatilityConfigVersionsResponse {
    result: Result;
    versions: CommodityVolatilityConfig[];
    total: number;
}

export interface GetCommodityVolatilityConfigVersionRequest {
    key: CommodityVolatilityConfigVersionKey;
}

export interface GetCommodityVolatilityConfigVersionResponse {
    result: Result;
    version: CommodityVolatilityConfig | null;
}

export const subjects = {
    list_commodity_volatility_configs_request: 'refdata.v1.commodity_volatility_configs.list',
    get_commodity_volatility_config_request: 'refdata.v1.commodity_volatility_configs.get',
    get_many_commodity_volatility_configs_request:
        'refdata.v1.commodity_volatility_configs.get_many',
    put_commodity_volatility_config_request: 'refdata.v1.commodity_volatility_configs.put',
    put_many_commodity_volatility_configs_request:
        'refdata.v1.commodity_volatility_configs.put_many',
    delete_commodity_volatility_config_request: 'refdata.v1.commodity_volatility_configs.delete',
    delete_many_commodity_volatility_configs_request:
        'refdata.v1.commodity_volatility_configs.delete_many',
    list_commodity_volatility_config_versions_request:
        'refdata.v1.commodity_volatility_configs_versions.list',
    get_commodity_volatility_config_version_request:
        'refdata.v1.commodity_volatility_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_volatility_configs_request: true,
    get_commodity_volatility_config_request: true,
    get_many_commodity_volatility_configs_request: true,
    put_commodity_volatility_config_request: true,
    put_many_commodity_volatility_configs_request: true,
    delete_commodity_volatility_config_request: true,
    delete_many_commodity_volatility_configs_request: true,
    list_commodity_volatility_config_versions_request: true,
    get_commodity_volatility_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_volatility_configs_events.created',
    updated: 'refdata.v1.commodity_volatility_configs_events.updated',
    deleted: 'refdata.v1.commodity_volatility_configs_events.deleted',
} as const;
