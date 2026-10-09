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
import type { MarketDataGenerationConfig } from '../domain/market_data_generation_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MarketDataGenerationConfigKey {
    id: string;
}

export interface MarketDataGenerationConfigWrite {
    id: string;
    scope: string;
    binding_mode: string;
    name: string;
    description: string;
    enabled: boolean;
    dataset_id: string | null;
}

export interface MarketDataGenerationConfigChange {
    write: MarketDataGenerationConfigWrite;
    precondition: Precondition;
}

export interface MarketDataGenerationConfigRemoval {
    key: MarketDataGenerationConfigKey;
    precondition: Precondition;
}

export interface MarketDataGenerationConfigLookup {
    key: MarketDataGenerationConfigKey;
    market_data_generation_config: MarketDataGenerationConfig | null;
}

export interface MarketDataGenerationConfigsFilter {
    id_one_of: string[] | null;
}

export interface MarketDataGenerationConfigEvent {
    event_id: string;
    key: MarketDataGenerationConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface MarketDataGenerationConfigVersionKey {
    market_data_generation_config: MarketDataGenerationConfigKey;
    version: number;
}

export interface MarketDataGenerationConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListMarketDataGenerationConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketDataGenerationConfigsFilter | null;
    as_of: string | null;
}

export interface ListMarketDataGenerationConfigsResponse {
    result: Result;
    market_data_generation_configs: MarketDataGenerationConfig[];
    total: number;
}

export interface GetMarketDataGenerationConfigRequest {
    key: MarketDataGenerationConfigKey;
}

export interface GetMarketDataGenerationConfigResponse {
    result: Result;
    market_data_generation_config: MarketDataGenerationConfig | null;
}

export interface GetManyMarketDataGenerationConfigsRequest {
    keys: MarketDataGenerationConfigKey[];
}

export interface GetManyMarketDataGenerationConfigsResponse {
    result: Result;
    entries: MarketDataGenerationConfigLookup[];
}

export interface PutMarketDataGenerationConfigRequest {
    change: MarketDataGenerationConfigChange;
    intent: ChangeIntent;
}

export interface PutMarketDataGenerationConfigResponse {
    result: Result;
    market_data_generation_config: MarketDataGenerationConfig | null;
}

export interface PutManyMarketDataGenerationConfigsRequest {
    changes: MarketDataGenerationConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketDataGenerationConfigsResponse {
    result: Result;
    market_data_generation_configs: MarketDataGenerationConfig[];
}

export interface DeleteMarketDataGenerationConfigRequest {
    removal: MarketDataGenerationConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketDataGenerationConfigResponse {
    result: Result;
}

export interface DeleteManyMarketDataGenerationConfigsRequest {
    removals: MarketDataGenerationConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketDataGenerationConfigsResponse {
    result: Result;
}

export interface ListMarketDataGenerationConfigVersionsRequest {
    key: MarketDataGenerationConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: MarketDataGenerationConfigVersionsFilter | null;
}

export interface ListMarketDataGenerationConfigVersionsResponse {
    result: Result;
    versions: MarketDataGenerationConfig[];
    total: number;
}

export interface GetMarketDataGenerationConfigVersionRequest {
    key: MarketDataGenerationConfigVersionKey;
}

export interface GetMarketDataGenerationConfigVersionResponse {
    result: Result;
    version: MarketDataGenerationConfig | null;
}

export const subjects = {
    list_market_data_generation_configs_request: 'synthetic.v1.market_data_generation_configs.list',
    get_market_data_generation_config_request: 'synthetic.v1.market_data_generation_configs.get',
    get_many_market_data_generation_configs_request:
        'synthetic.v1.market_data_generation_configs.get_many',
    put_market_data_generation_config_request: 'synthetic.v1.market_data_generation_configs.put',
    put_many_market_data_generation_configs_request:
        'synthetic.v1.market_data_generation_configs.put_many',
    delete_market_data_generation_config_request:
        'synthetic.v1.market_data_generation_configs.delete',
    delete_many_market_data_generation_configs_request:
        'synthetic.v1.market_data_generation_configs.delete_many',
    list_market_data_generation_config_versions_request:
        'synthetic.v1.market_data_generation_configs_versions.list',
    get_market_data_generation_config_version_request:
        'synthetic.v1.market_data_generation_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_data_generation_configs_request: true,
    get_market_data_generation_config_request: true,
    get_many_market_data_generation_configs_request: true,
    put_market_data_generation_config_request: true,
    put_many_market_data_generation_configs_request: true,
    delete_market_data_generation_config_request: true,
    delete_many_market_data_generation_configs_request: true,
    list_market_data_generation_config_versions_request: true,
    get_market_data_generation_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.market_data_generation_configs_events.created',
    updated: 'synthetic.v1.market_data_generation_configs_events.updated',
    deleted: 'synthetic.v1.market_data_generation_configs_events.deleted',
} as const;
