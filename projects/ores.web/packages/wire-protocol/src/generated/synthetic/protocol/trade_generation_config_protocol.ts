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
import type { TradeGenerationConfig } from '../domain/trade_generation_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeGenerationConfigKey {
    id: string;
}

export interface TradeGenerationConfigWrite {
    id: string;
    name: string;
    description: string;
    asset_class_code: string;
    min_tenor_code: string;
    max_tenor_code: string;
    min_notional: string;
    max_notional: string;
    as_of_date: string;
    seed: number;
    trades_per_book: number;
    enabled: boolean;
    reference_curve_id: string;
}

export interface TradeGenerationConfigChange {
    write: TradeGenerationConfigWrite;
    precondition: Precondition;
}

export interface TradeGenerationConfigRemoval {
    key: TradeGenerationConfigKey;
    precondition: Precondition;
}

export interface TradeGenerationConfigLookup {
    key: TradeGenerationConfigKey;
    trade_generation_config: TradeGenerationConfig | null;
}

export interface TradeGenerationConfigsFilter {
    as_of_date: string | null;
    id_one_of: string[] | null;
    as_of_date_one_of: string[] | null;
    as_of_date_from: string | null;
    as_of_date_to: string | null;
    search: string | null;
}

export interface TradeGenerationConfigEvent {
    event_id: string;
    key: TradeGenerationConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeGenerationConfigVersionKey {
    trade_generation_config: TradeGenerationConfigKey;
    version: number;
}

export interface TradeGenerationConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeGenerationConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeGenerationConfigsFilter | null;
    as_of: string | null;
}

export interface ListTradeGenerationConfigsResponse {
    result: Result;
    trade_generation_configs: TradeGenerationConfig[];
    total: number;
}

export interface GetTradeGenerationConfigRequest {
    key: TradeGenerationConfigKey;
}

export interface GetTradeGenerationConfigResponse {
    result: Result;
    trade_generation_config: TradeGenerationConfig | null;
}

export interface GetManyTradeGenerationConfigsRequest {
    keys: TradeGenerationConfigKey[];
}

export interface GetManyTradeGenerationConfigsResponse {
    result: Result;
    entries: TradeGenerationConfigLookup[];
}

export interface PutTradeGenerationConfigRequest {
    change: TradeGenerationConfigChange;
    intent: ChangeIntent;
}

export interface PutTradeGenerationConfigResponse {
    result: Result;
    trade_generation_config: TradeGenerationConfig | null;
}

export interface PutManyTradeGenerationConfigsRequest {
    changes: TradeGenerationConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeGenerationConfigsResponse {
    result: Result;
    trade_generation_configs: TradeGenerationConfig[];
}

export interface DeleteTradeGenerationConfigRequest {
    removal: TradeGenerationConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeGenerationConfigResponse {
    result: Result;
}

export interface DeleteManyTradeGenerationConfigsRequest {
    removals: TradeGenerationConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeGenerationConfigsResponse {
    result: Result;
}

export interface ListTradeGenerationConfigVersionsRequest {
    key: TradeGenerationConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeGenerationConfigVersionsFilter | null;
}

export interface ListTradeGenerationConfigVersionsResponse {
    result: Result;
    versions: TradeGenerationConfig[];
    total: number;
}

export interface GetTradeGenerationConfigVersionRequest {
    key: TradeGenerationConfigVersionKey;
}

export interface GetTradeGenerationConfigVersionResponse {
    result: Result;
    version: TradeGenerationConfig | null;
}

export const subjects = {
    list_trade_generation_configs_request: 'synthetic.v1.trade_generation_configs.list',
    get_trade_generation_config_request: 'synthetic.v1.trade_generation_configs.get',
    get_many_trade_generation_configs_request: 'synthetic.v1.trade_generation_configs.get_many',
    put_trade_generation_config_request: 'synthetic.v1.trade_generation_configs.put',
    put_many_trade_generation_configs_request: 'synthetic.v1.trade_generation_configs.put_many',
    delete_trade_generation_config_request: 'synthetic.v1.trade_generation_configs.delete',
    delete_many_trade_generation_configs_request:
        'synthetic.v1.trade_generation_configs.delete_many',
    list_trade_generation_config_versions_request:
        'synthetic.v1.trade_generation_configs_versions.list',
    get_trade_generation_config_version_request:
        'synthetic.v1.trade_generation_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_generation_configs_request: true,
    get_trade_generation_config_request: true,
    get_many_trade_generation_configs_request: true,
    put_trade_generation_config_request: true,
    put_many_trade_generation_configs_request: true,
    delete_trade_generation_config_request: true,
    delete_many_trade_generation_configs_request: true,
    list_trade_generation_config_versions_request: true,
    get_trade_generation_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.trade_generation_configs_events.created',
    updated: 'synthetic.v1.trade_generation_configs_events.updated',
    deleted: 'synthetic.v1.trade_generation_configs_events.deleted',
} as const;
