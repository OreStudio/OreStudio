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
import type { TodaysMarketConfiguration } from '../domain/todays_market_configuration.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TodaysMarketConfigurationKey {
    configuration_id: string;
}

export interface TodaysMarketConfigurationWrite {
    id: string;
    todays_market_config_id: string;
    configuration_id: string;
    position: number;
}

export interface TodaysMarketConfigurationChange {
    write: TodaysMarketConfigurationWrite;
    precondition: Precondition;
}

export interface TodaysMarketConfigurationRemoval {
    key: TodaysMarketConfigurationKey;
    precondition: Precondition;
}

export interface TodaysMarketConfigurationLookup {
    key: TodaysMarketConfigurationKey;
    todays_market_configuration: TodaysMarketConfiguration | null;
}

export interface TodaysMarketConfigurationsFilter {
    todays_market_config_id: string | null;
    id_one_of: string[] | null;
    todays_market_config_id_one_of: string[] | null;
}

export interface TodaysMarketConfigurationEvent {
    event_id: string;
    key: TodaysMarketConfigurationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TodaysMarketConfigurationVersionKey {
    todays_market_configuration: TodaysMarketConfigurationKey;
    version: number;
}

export interface TodaysMarketConfigurationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTodaysMarketConfigurationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketConfigurationsFilter | null;
}

export interface ListTodaysMarketConfigurationsResponse {
    result: Result;
    configurations: TodaysMarketConfiguration[];
    total: number;
}

export interface GetTodaysMarketConfigurationRequest {
    key: TodaysMarketConfigurationKey;
}

export interface GetTodaysMarketConfigurationResponse {
    result: Result;
    todays_market_configuration: TodaysMarketConfiguration | null;
}

export interface GetManyTodaysMarketConfigurationsRequest {
    keys: TodaysMarketConfigurationKey[];
}

export interface GetManyTodaysMarketConfigurationsResponse {
    result: Result;
    entries: TodaysMarketConfigurationLookup[];
}

export interface PutTodaysMarketConfigurationRequest {
    change: TodaysMarketConfigurationChange;
    intent: ChangeIntent;
}

export interface PutTodaysMarketConfigurationResponse {
    result: Result;
    todays_market_configuration: TodaysMarketConfiguration | null;
}

export interface PutManyTodaysMarketConfigurationsRequest {
    changes: TodaysMarketConfigurationChange[];
    intent: ChangeIntent;
}

export interface PutManyTodaysMarketConfigurationsResponse {
    result: Result;
    configurations: TodaysMarketConfiguration[];
}

export interface DeleteTodaysMarketConfigurationRequest {
    removal: TodaysMarketConfigurationRemoval;
    intent: ChangeIntent;
}

export interface DeleteTodaysMarketConfigurationResponse {
    result: Result;
}

export interface DeleteManyTodaysMarketConfigurationsRequest {
    removals: TodaysMarketConfigurationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTodaysMarketConfigurationsResponse {
    result: Result;
}

export interface ListByTodaysMarketConfigIdTodaysMarketConfigurationsRequest {
    todays_market_config_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketConfigurationsFilter | null;
}

export interface ListByTodaysMarketConfigIdTodaysMarketConfigurationsResponse {
    result: Result;
    configurations: TodaysMarketConfiguration[];
    total: number;
}

export interface ListTodaysMarketConfigurationVersionsRequest {
    key: TodaysMarketConfigurationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketConfigurationVersionsFilter | null;
}

export interface ListTodaysMarketConfigurationVersionsResponse {
    result: Result;
    versions: TodaysMarketConfiguration[];
    total: number;
}

export interface GetTodaysMarketConfigurationVersionRequest {
    key: TodaysMarketConfigurationVersionKey;
}

export interface GetTodaysMarketConfigurationVersionResponse {
    result: Result;
    version: TodaysMarketConfiguration | null;
}

export const subjects = {
    list_todays_market_configurations_request: 'analytics.v1.todays_market_configurations.list',
    get_todays_market_configuration_request: 'analytics.v1.todays_market_configurations.get',
    get_many_todays_market_configurations_request:
        'analytics.v1.todays_market_configurations.get_many',
    put_todays_market_configuration_request: 'analytics.v1.todays_market_configurations.put',
    put_many_todays_market_configurations_request:
        'analytics.v1.todays_market_configurations.put_many',
    delete_todays_market_configuration_request: 'analytics.v1.todays_market_configurations.delete',
    delete_many_todays_market_configurations_request:
        'analytics.v1.todays_market_configurations.delete_many',
    list_by_todays_market_config_id_todays_market_configurations_request:
        'analytics.v1.todays_market_configurations.list_by_todays_market_config_id',
    list_todays_market_configuration_versions_request:
        'analytics.v1.todays_market_configurations_versions.list',
    get_todays_market_configuration_version_request:
        'analytics.v1.todays_market_configurations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_todays_market_configurations_request: true,
    get_todays_market_configuration_request: true,
    get_many_todays_market_configurations_request: true,
    put_todays_market_configuration_request: true,
    put_many_todays_market_configurations_request: true,
    delete_todays_market_configuration_request: true,
    delete_many_todays_market_configurations_request: true,
    list_by_todays_market_config_id_todays_market_configurations_request: true,
    list_todays_market_configuration_versions_request: true,
    get_todays_market_configuration_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.todays_market_configurations_events.created',
    updated: 'analytics.v1.todays_market_configurations_events.updated',
    deleted: 'analytics.v1.todays_market_configurations_events.deleted',
} as const;
