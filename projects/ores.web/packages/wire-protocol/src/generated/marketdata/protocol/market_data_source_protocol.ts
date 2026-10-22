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
import type { MarketDataSource } from '../domain/market_data_source.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MarketDataSourceKey {
    name: string;
}

export interface MarketDataSourceWrite {
    id: string;
    name: string;
    provider_code: string;
    kind: string;
    is_default: boolean;
    description: string;
}

export interface MarketDataSourceChange {
    write: MarketDataSourceWrite;
    precondition: Precondition;
}

export interface MarketDataSourceRemoval {
    key: MarketDataSourceKey;
    precondition: Precondition;
}

export interface MarketDataSourceLookup {
    key: MarketDataSourceKey;
    market_data_source: MarketDataSource | null;
}

export interface MarketDataSourcesFilter {
    provider_code: string | null;
    kind: string | null;
    id_one_of: string[] | null;
    provider_code_one_of: string[] | null;
    kind_one_of: string[] | null;
    search: string | null;
}

export interface MarketDataSourceEvent {
    event_id: string;
    key: MarketDataSourceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface MarketDataSourceVersionKey {
    market_data_source: MarketDataSourceKey;
    version: number;
}

export interface MarketDataSourceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListMarketDataSourcesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketDataSourcesFilter | null;
    as_of: string | null;
}

export interface ListMarketDataSourcesResponse {
    result: Result;
    sources: MarketDataSource[];
    total: number;
}

export interface GetMarketDataSourceRequest {
    key: MarketDataSourceKey;
}

export interface GetMarketDataSourceResponse {
    result: Result;
    market_data_source: MarketDataSource | null;
}

export interface GetManyMarketDataSourcesRequest {
    keys: MarketDataSourceKey[];
}

export interface GetManyMarketDataSourcesResponse {
    result: Result;
    entries: MarketDataSourceLookup[];
}

export interface PutMarketDataSourceRequest {
    change: MarketDataSourceChange;
    intent: ChangeIntent;
}

export interface PutMarketDataSourceResponse {
    result: Result;
    market_data_source: MarketDataSource | null;
}

export interface PutManyMarketDataSourcesRequest {
    changes: MarketDataSourceChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketDataSourcesResponse {
    result: Result;
    sources: MarketDataSource[];
}

export interface DeleteMarketDataSourceRequest {
    removal: MarketDataSourceRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketDataSourceResponse {
    result: Result;
}

export interface DeleteManyMarketDataSourcesRequest {
    removals: MarketDataSourceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketDataSourcesResponse {
    result: Result;
}

export interface ListMarketDataSourceVersionsRequest {
    key: MarketDataSourceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: MarketDataSourceVersionsFilter | null;
}

export interface ListMarketDataSourceVersionsResponse {
    result: Result;
    versions: MarketDataSource[];
    total: number;
}

export interface GetMarketDataSourceVersionRequest {
    key: MarketDataSourceVersionKey;
}

export interface GetMarketDataSourceVersionResponse {
    result: Result;
    version: MarketDataSource | null;
}

export const subjects = {
    list_market_data_sources_request: 'marketdata.v1.market_data_sources.list',
    get_market_data_source_request: 'marketdata.v1.market_data_sources.get',
    get_many_market_data_sources_request: 'marketdata.v1.market_data_sources.get_many',
    put_market_data_source_request: 'marketdata.v1.market_data_sources.put',
    put_many_market_data_sources_request: 'marketdata.v1.market_data_sources.put_many',
    delete_market_data_source_request: 'marketdata.v1.market_data_sources.delete',
    delete_many_market_data_sources_request: 'marketdata.v1.market_data_sources.delete_many',
    list_market_data_source_versions_request: 'marketdata.v1.market_data_sources_versions.list',
    get_market_data_source_version_request: 'marketdata.v1.market_data_sources_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_data_sources_request: true,
    get_market_data_source_request: true,
    get_many_market_data_sources_request: true,
    put_market_data_source_request: true,
    put_many_market_data_sources_request: true,
    delete_market_data_source_request: true,
    delete_many_market_data_sources_request: true,
    list_market_data_source_versions_request: true,
    get_market_data_source_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.market_data_sources_events.created',
    updated: 'marketdata.v1.market_data_sources_events.updated',
    deleted: 'marketdata.v1.market_data_sources_events.deleted',
} as const;
