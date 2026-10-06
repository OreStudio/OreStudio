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
import type { TodaysMarketConfig } from '../domain/todays_market_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TodaysMarketConfigKey {
    name: string;
}

export interface TodaysMarketConfigWrite {
    id: string;
    party_id: string;
    name: string;
    description: string;
    config_variant: string;
    configuration_id: string;
}

export interface TodaysMarketConfigChange {
    write: TodaysMarketConfigWrite;
    precondition: Precondition;
}

export interface TodaysMarketConfigRemoval {
    key: TodaysMarketConfigKey;
    precondition: Precondition;
}

export interface TodaysMarketConfigLookup {
    key: TodaysMarketConfigKey;
    todays_market_config: TodaysMarketConfig | null;
}

export interface TodaysMarketConfigsFilter {
    configuration_id: string | null;
    id_one_of: string[] | null;
    configuration_id_one_of: string[] | null;
}

export interface TodaysMarketConfigEvent {
    event_id: string;
    key: TodaysMarketConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TodaysMarketConfigVersionKey {
    todays_market_config: TodaysMarketConfigKey;
    version: number;
}

export interface TodaysMarketConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTodaysMarketConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketConfigsFilter | null;
    as_of: string | null;
}

export interface ListTodaysMarketConfigsResponse {
    result: Result;
    configs: TodaysMarketConfig[];
    total: number;
}

export interface GetTodaysMarketConfigRequest {
    key: TodaysMarketConfigKey;
}

export interface GetTodaysMarketConfigResponse {
    result: Result;
    todays_market_config: TodaysMarketConfig | null;
}

export interface GetManyTodaysMarketConfigsRequest {
    keys: TodaysMarketConfigKey[];
}

export interface GetManyTodaysMarketConfigsResponse {
    result: Result;
    entries: TodaysMarketConfigLookup[];
}

export interface PutTodaysMarketConfigRequest {
    change: TodaysMarketConfigChange;
    intent: ChangeIntent;
}

export interface PutTodaysMarketConfigResponse {
    result: Result;
    todays_market_config: TodaysMarketConfig | null;
}

export interface PutManyTodaysMarketConfigsRequest {
    changes: TodaysMarketConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyTodaysMarketConfigsResponse {
    result: Result;
    configs: TodaysMarketConfig[];
}

export interface DeleteTodaysMarketConfigRequest {
    removal: TodaysMarketConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteTodaysMarketConfigResponse {
    result: Result;
}

export interface DeleteManyTodaysMarketConfigsRequest {
    removals: TodaysMarketConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTodaysMarketConfigsResponse {
    result: Result;
}

export interface ListByConfigurationIdTodaysMarketConfigsRequest {
    configuration_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketConfigsFilter | null;
}

export interface ListByConfigurationIdTodaysMarketConfigsResponse {
    result: Result;
    configs: TodaysMarketConfig[];
    total: number;
}

export interface ListTodaysMarketConfigVersionsRequest {
    key: TodaysMarketConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketConfigVersionsFilter | null;
}

export interface ListTodaysMarketConfigVersionsResponse {
    result: Result;
    versions: TodaysMarketConfig[];
    total: number;
}

export interface GetTodaysMarketConfigVersionRequest {
    key: TodaysMarketConfigVersionKey;
}

export interface GetTodaysMarketConfigVersionResponse {
    result: Result;
    version: TodaysMarketConfig | null;
}

export const subjects = {
    list_todays_market_configs_request: 'analytics.v1.todays_market_configs.list',
    get_todays_market_config_request: 'analytics.v1.todays_market_configs.get',
    get_many_todays_market_configs_request: 'analytics.v1.todays_market_configs.get_many',
    put_todays_market_config_request: 'analytics.v1.todays_market_configs.put',
    put_many_todays_market_configs_request: 'analytics.v1.todays_market_configs.put_many',
    delete_todays_market_config_request: 'analytics.v1.todays_market_configs.delete',
    delete_many_todays_market_configs_request: 'analytics.v1.todays_market_configs.delete_many',
    list_by_configuration_id_todays_market_configs_request:
        'analytics.v1.todays_market_configs.list_by_configuration_id',
    list_todays_market_config_versions_request: 'analytics.v1.todays_market_configs_versions.list',
    get_todays_market_config_version_request: 'analytics.v1.todays_market_configs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_todays_market_configs_request: true,
    get_todays_market_config_request: true,
    get_many_todays_market_configs_request: true,
    put_todays_market_config_request: true,
    put_many_todays_market_configs_request: true,
    delete_todays_market_config_request: true,
    delete_many_todays_market_configs_request: true,
    list_by_configuration_id_todays_market_configs_request: true,
    list_todays_market_config_versions_request: true,
    get_todays_market_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.todays_market_configs_events.created',
    updated: 'analytics.v1.todays_market_configs_events.updated',
    deleted: 'analytics.v1.todays_market_configs_events.deleted',
} as const;
