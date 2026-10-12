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
import type { MarketDataProvider } from '../domain/market_data_provider.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MarketDataProviderKey {
    code: string;
}

export interface MarketDataProviderWrite {
    code: string;
    name: string;
    kind: string;
    description: string;
    display_order: number;
}

export interface MarketDataProviderChange {
    write: MarketDataProviderWrite;
    precondition: Precondition;
}

export interface MarketDataProviderRemoval {
    key: MarketDataProviderKey;
    precondition: Precondition;
}

export interface MarketDataProviderLookup {
    key: MarketDataProviderKey;
    market_data_provider: MarketDataProvider | null;
}

export interface MarketDataProvidersFilter {
    code_one_of: string[] | null;
}

export interface MarketDataProviderEvent {
    event_id: string;
    key: MarketDataProviderKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface MarketDataProviderVersionKey {
    market_data_provider: MarketDataProviderKey;
    version: number;
}

export interface MarketDataProviderVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListMarketDataProvidersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketDataProvidersFilter | null;
    as_of: string | null;
}

export interface ListMarketDataProvidersResponse {
    result: Result;
    providers: MarketDataProvider[];
    total: number;
}

export interface GetMarketDataProviderRequest {
    key: MarketDataProviderKey;
}

export interface GetMarketDataProviderResponse {
    result: Result;
    market_data_provider: MarketDataProvider | null;
}

export interface GetManyMarketDataProvidersRequest {
    keys: MarketDataProviderKey[];
}

export interface GetManyMarketDataProvidersResponse {
    result: Result;
    entries: MarketDataProviderLookup[];
}

export interface PutMarketDataProviderRequest {
    change: MarketDataProviderChange;
    intent: ChangeIntent;
}

export interface PutMarketDataProviderResponse {
    result: Result;
    market_data_provider: MarketDataProvider | null;
}

export interface PutManyMarketDataProvidersRequest {
    changes: MarketDataProviderChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketDataProvidersResponse {
    result: Result;
    providers: MarketDataProvider[];
}

export interface DeleteMarketDataProviderRequest {
    removal: MarketDataProviderRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketDataProviderResponse {
    result: Result;
}

export interface DeleteManyMarketDataProvidersRequest {
    removals: MarketDataProviderRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketDataProvidersResponse {
    result: Result;
}

export interface ListMarketDataProviderVersionsRequest {
    key: MarketDataProviderKey;
    offset: number;
    limit: number;
    order: Order;
    filter: MarketDataProviderVersionsFilter | null;
}

export interface ListMarketDataProviderVersionsResponse {
    result: Result;
    versions: MarketDataProvider[];
    total: number;
}

export interface GetMarketDataProviderVersionRequest {
    key: MarketDataProviderVersionKey;
}

export interface GetMarketDataProviderVersionResponse {
    result: Result;
    version: MarketDataProvider | null;
}

export const subjects = {
    list_market_data_providers_request: 'synthetic.v1.market_data_providers.list',
    get_market_data_provider_request: 'synthetic.v1.market_data_providers.get',
    get_many_market_data_providers_request: 'synthetic.v1.market_data_providers.get_many',
    put_market_data_provider_request: 'synthetic.v1.market_data_providers.put',
    put_many_market_data_providers_request: 'synthetic.v1.market_data_providers.put_many',
    delete_market_data_provider_request: 'synthetic.v1.market_data_providers.delete',
    delete_many_market_data_providers_request: 'synthetic.v1.market_data_providers.delete_many',
    list_market_data_provider_versions_request: 'synthetic.v1.market_data_providers_versions.list',
    get_market_data_provider_version_request: 'synthetic.v1.market_data_providers_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_data_providers_request: true,
    get_market_data_provider_request: true,
    get_many_market_data_providers_request: true,
    put_market_data_provider_request: true,
    put_many_market_data_providers_request: true,
    delete_market_data_provider_request: true,
    delete_many_market_data_providers_request: true,
    list_market_data_provider_versions_request: true,
    get_market_data_provider_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.market_data_providers_events.created',
    updated: 'synthetic.v1.market_data_providers_events.updated',
    deleted: 'synthetic.v1.market_data_providers_events.deleted',
} as const;
